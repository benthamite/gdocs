# gdocs.el — Google Docs integration for Emacs org-mode

A comprehensive Emacs package that enables bidirectional synchronization between org-mode files and Google Docs, allowing users to read, write, and collaborate on Google Docs entirely from within Emacs.

## Use cases

1. **Publish org to Google Docs**: Create a Google Doc from an existing org file to share with others. The two files remain in sync — edits to either propagate to the other.
2. **Read/edit shared Google Docs**: Open a Google Doc shared by someone else as an org-mode buffer in Emacs. Edit or suggest changes without leaving Emacs.
3. **Import existing Google Docs**: Import old Google Docs into org files. The files remain synchronized, but the user may later choose to delete the Google Doc.

## Package overview

- **Package name**: `gdocs`
- **Command prefix**: `gdocs-`
- **Language**: Pure Emacs Lisp
- **Dependencies**: `plz.el` (async HTTP), `dash.el` (list utilities), `s.el` (string utilities), `org` (built-in)
- **Minimum Emacs version**: 29.1

## Architecture

The package is composed of the following modules:

```
gdocs.el                  ; Entry point, user commands, minor mode
gdocs-auth.el             ; OAuth 2.0 flow, token management, multi-account
gdocs-api.el              ; Google Docs & Drive API client (async via plz.el)
gdocs-convert.el          ; Bidirectional org <-> Google Docs format conversion
gdocs-diff.el             ; Incremental diff engine (org changes -> API batchUpdate requests)
gdocs-sync.el             ; Sync engine (push, pull, conflict detection, polling)
gdocs-comments.el         ; Comments and suggestions system
gdocs-drive.el            ; Google Drive browser (dired-like)
gdocs-export.el           ; Org export dispatcher backend
gdocs-merge.el            ; Conflict resolution UI (side-by-side diff view)
```

---

## Phase 1: MVP — Core sync

The MVP delivers the essential round-trip: open a Google Doc as org, edit it, push changes back. Also: create a Google Doc from an org file.

### 1.1 Authentication (`gdocs-auth.el`)

#### OAuth 2.0 flow

- Ship with a built-in Google Cloud OAuth client ID and secret for zero-config quick start.
- The OAuth flow:
  1. Open the user's default browser to Google's OAuth consent screen.
  2. Start a temporary local HTTP server (on a random high port) to receive the authorization code callback.
  3. Exchange the code for access and refresh tokens.
  4. Store tokens securely on disk (see below).
- Scopes required:
  - `https://www.googleapis.com/auth/documents` (read/write Docs)
  - `https://www.googleapis.com/auth/drive` (list/browse/create files)
- Token refresh: automatically refresh expired access tokens using the stored refresh token before each API call.

#### Multi-account support

- Support multiple named Google accounts.
- Each account stores its own OAuth tokens.
- Configuration:

  ```elisp
  (setq gdocs-accounts
        '(("personal" . ((client-id . "built-in")
                         (client-secret . "built-in")))
          ("work" . ((client-id . "user-provided-id")
                     (client-secret . "user-provided-secret")))))
  ```

- When `gdocs-accounts` is nil or has only one entry, the package uses that account without prompting.
- When multiple accounts exist, commands that operate on unlinked documents prompt the user to select an account via `completing-read`.
- Each linked org file remembers which account it is associated with (stored in metadata).
- Commands:
  - `gdocs-authenticate` — Initiate OAuth flow for a given account.
  - `gdocs-logout` — Revoke tokens for an account.

#### Token storage

- Tokens stored in `~/.emacs.d/gdocs/tokens/` (or `user-emacs-directory`), one JSON file per account.
- Files created with restrictive permissions (600).
- Optionally integrate with `auth-source` for users who prefer keychain/GPG-based storage.
- Custom variable: `gdocs-token-directory`.

#### Custom client credentials

- Users can override the built-in credentials per account by setting `client-id` and `client-secret` in `gdocs-accounts`.
- This is recommended for heavy usage to avoid shared rate limits.

### 1.2 Google API client (`gdocs-api.el`)

All API calls are fully asynchronous via `plz.el` to avoid blocking Emacs.

#### Google Docs API

- `gdocs-api-get-document (document-id callback &optional account)` — Fetch a document's JSON structure.
- `gdocs-api-batch-update (document-id requests callback &optional account)` — Send a list of batchUpdate requests.
- `gdocs-api-create-document (title callback &optional account)` — Create a new empty document.

#### Google Drive API

- `gdocs-api-list-files (query callback &optional account page-token)` — List files matching a query (supports pagination).
- `gdocs-api-get-file-metadata (file-id callback &optional account)` — Get file metadata (title, revision, permissions).
- `gdocs-api-upload-image (file-path callback &optional account folder-id)` — Upload a local image to Google Drive.
- `gdocs-api-search-files (query callback &optional account)` — Full-text search across Drive.

#### Error handling

- Retry with exponential backoff on transient errors (429, 500, 503).
- Surface clear error messages for auth failures (401/403) with instructions to re-authenticate.
- Rate limit awareness: respect `Retry-After` headers.

#### Progress indication

- Show progress in the modeline during API operations (e.g., "Syncing to Google Docs...").
- Use `plz.el` progress callbacks where available.

### 1.3 Format conversion (`gdocs-convert.el`)

Bidirectional conversion between org-mode syntax and Google Docs JSON document structure.

#### Org parser

Use `org-element-parse-buffer` to parse org content into a structured AST. All conversion works on the AST level, not raw text.

#### Org -> Google Docs mapping

| Org element | Google Docs equivalent |
|---|---|
| `#+TITLE:` | Title paragraph style |
| `* Heading` (level 1-6) | Heading 1-6 paragraph style |
| `* Heading` (level 7+) | Heading 6 (flattened) |
| Body text / paragraphs | Normal text paragraphs |
| `*bold*` | Bold text run |
| `/italic/` | Italic text run |
| `_underline_` | Underline text run |
| `+strikethrough+` | Strikethrough text run |
| `=verbatim=` / `~code~` | Monospace font text run |
| `[[url][description]]` | Hyperlink |
| Unordered lists (`-`, `+`) | Bullet list |
| Ordered lists (`1.`, `1)`) | Numbered list |
| Nested lists | Nested list with appropriate nesting level |
| Checkbox items (`[ ]`, `[X]`) | Checklist items (Google Docs supports these natively) |
| Tables | Google Docs table |
| Horizontal rules (`-----`) | Horizontal rule (section break with no header/footer) |
| Block quotes (`#+BEGIN_QUOTE`) | Indented paragraph or custom style |
| Footnotes (`[fn:1]`) | Google Docs footnote |
| Images (`[[file:img.png]]`) | Inline image (uploaded to Drive, embedded by URL) |

#### Org-only constructs (no Google Docs equivalent)

These are preserved as special markers in the Google Docs so they survive round-trips without loss:

- **TODO keywords**: Wrapped in a zero-width invisible marker comment. Rendered as styled text (e.g., bold colored) in the Doc for readability, with a corresponding comment anchor containing the raw org syntax.
- **Tags**: Same treatment — rendered as styled text with a preservation comment.
- **Timestamps** (`<2024-01-15 Mon>`, `SCHEDULED:`, `DEADLINE:`): Preserved as markers.
- **Properties and drawers** (`:PROPERTIES:`, `:LOGBOOK:`): Preserved as markers. Not rendered visually in the Doc.
- **Source code blocks** (`#+BEGIN_SRC`): Rendered as monospace-formatted text with language annotation in a comment. The full org source block syntax is preserved in the comment.
- **LaTeX fragments**: Rendered as monospace text with the raw LaTeX in a comment.
- **Org keywords** (`#+AUTHOR:`, `#+DATE:`, etc.): Preserved as markers at the top of the document.
- **Custom IDs and anchors**: Preserved as comments.

The marker format: a Google Docs comment anchored to the relevant text, with the body containing a machine-readable tag like `[gdocs:org-element:BEGIN_SRC python]...code...[gdocs:org-element:END_SRC]`. This allows the converter to reconstruct the original org syntax on pull.

#### Google Docs-only constructs (no org equivalent)

Preserved as opaque org markers so they survive round-trips:

- **Page breaks**: `#+GDOCS_PAGE_BREAK:`
- **Headers/footers**: `#+GDOCS_HEADER:` / `#+GDOCS_FOOTER:` blocks
- **Page numbers**: `#+GDOCS_PAGE_NUMBER:`
- **Table of contents**: `#+GDOCS_TOC:`
- **Embedded drawings**: `#+GDOCS_DRAWING:` with a serialized reference
- **Charts**: `#+GDOCS_CHART:` with a serialized reference
- **Named styles / custom formatting**: Preserved as inline markers

#### Google Docs -> Org conversion

The inverse of the above table. The converter:

1. Walks the Google Docs JSON document structure (body -> content -> structural elements).
2. Maps each structural element to its org equivalent.
3. Reconstructs org-only constructs from their marker comments.
4. Preserves Google Docs-only constructs as opaque markers.
5. Outputs well-formatted org text.

### 1.4 Sync engine (`gdocs-sync.el`)

#### Metadata storage

Each linked org file stores sync metadata in file-local variables at the end of the file:

```org
# Local Variables:
# gdocs-document-id: "1BxiMVs0XRA5nFMdKvBdBZjgmUUqptlbs74OgVE2upms"
# gdocs-account: "personal"
# gdocs-revision-id: "ALm37BVT..."
# gdocs-last-sync: "2024-01-15T10:30:00Z"
# End:
```

#### Shadow copy

- The last-synced org content is kept in memory (in a buffer-local variable) for computing incremental diffs.
- On first open of a linked file (or after Emacs restart), if no shadow is in memory, the next push triggers a full document replacement instead of an incremental diff. A message warns the user: "No sync baseline in memory. Next push will do a full replacement."
- Alternatively, on first open, the package can pull the current Google Doc state to establish the shadow baseline.

#### Push (org -> Google Docs)

Triggered automatically on `after-save-hook` for buffers with `gdocs-mode` active:

1. Parse the current buffer with `org-element-parse-buffer`.
2. Compare against the shadow AST (last-synced state).
3. Generate a list of Google Docs API `batchUpdate` requests representing the diff (see section 1.5).
4. Send the requests asynchronously.
5. On success: update the shadow copy and revision ID. Show "Pushed to Google Docs" in the echo area.
6. On failure: show error, do not update shadow. The next save will retry.

#### Pull (Google Docs -> org)

Triggered explicitly via `gdocs-pull` command:

1. Fetch the document's current revision ID via `gdocs-api-get-file-metadata`.
2. If the revision matches the stored one, no changes — report "Already up to date."
3. If different, fetch the full document JSON via `gdocs-api-get-document`.
4. Convert to org format via `gdocs-convert`.
5. Check for local unsaved changes (conflicts — see section 1.8).
6. If no conflicts: replace buffer content, update shadow and revision ID.
7. If conflicts: trigger conflict resolution (see section 1.8).

#### Linking and unlinking

- `gdocs-link (document-id-or-url)` — Link the current org buffer to an existing Google Doc. Prompts for account if multiple. Writes file-local variables. Does an initial pull to establish the shadow.
- `gdocs-unlink` — Remove the file-local variables. The org file becomes a standalone file.

### 1.5 Incremental diff engine (`gdocs-diff.el`)

Computes the minimal set of Google Docs API `batchUpdate` requests to transform the remote document from its last-known state to the current org state.

#### Approach

1. Produce a flat list of "document elements" from both the old and new org ASTs. Each element is a paragraph, list item, table, image, etc., with its formatting.
2. Run a longest-common-subsequence (LCS) diff on these element lists.
3. Map diff operations to Google Docs API requests:
   - **Insert**: `InsertText`, `InsertTable`, `InsertInlineImage`, `CreateParagraphBullets`, etc.
   - **Delete**: `DeleteContentRange`
   - **Modify** (same element, changed content/formatting): `DeleteContentRange` + re-insert, or targeted `UpdateTextStyle` / `UpdateParagraphStyle`.
4. Order requests correctly. The Google Docs API processes requests sequentially and indices shift. Process deletions in reverse order (highest index first), then insertions in forward order.

#### Formatting updates

- Text style changes (bold, italic, etc.) use `UpdateTextStyle` with a range.
- Paragraph style changes (heading level, list type) use `UpdateParagraphStyle`.
- These can be applied as targeted updates without deleting/re-inserting content.

#### Fallback

If the diff engine encounters a situation it cannot handle (e.g., extremely complex restructuring), it falls back to full document replacement with a user warning.

### 1.6 Core user commands

- `gdocs-open (document-id-or-url)` — Fetch a Google Doc, convert to org, open in a buffer with `gdocs-mode` enabled. The buffer is saved to the configured local directory.
- `gdocs-create` — Create a new Google Doc from the current org buffer. Prompts for title and account. Links the buffer.
- `gdocs-push` — Manually push changes (normally automatic on save).
- `gdocs-pull` — Pull remote changes into the buffer.
- `gdocs-link` — Link current buffer to an existing Google Doc.
- `gdocs-unlink` — Remove the link.
- `gdocs-status` — Show sync status in the echo area (last sync time, revision, dirty state).
- `gdocs-open-in-browser` — Open the linked Google Doc in the default web browser.

### 1.7 Minor mode (`gdocs-mode`)

A buffer-local minor mode activated on linked org buffers:

- Adds `gdocs--push-on-save` to `after-save-hook`.
- Starts the periodic polling timer (see section 2.3).
- Modeline lighter: ` GDocs` with a sync status indicator:
  - ` GDocs:synced` — in sync
  - ` GDocs:modified` — local changes not yet pushed
  - ` GDocs:conflict` — remote changes detected, conflicts pending
  - ` GDocs:error` — last sync failed

### 1.8 Conflict resolution (`gdocs-merge.el`)

When both local and remote changes are detected (local buffer is dirty and remote revision has changed):

1. Show a side-by-side diff view in a dedicated buffer.
2. Left side: local org content. Right side: remote org content (converted from Google Docs).
3. Changes are highlighted with faces (additions in green, deletions in red).
4. Per-hunk controls:
   - `a` — accept local version of this hunk
   - `b` — accept remote version of this hunk
   - `e` — edit the merged result manually
5. Global controls:
   - `A` — accept all local
   - `B` — accept all remote
   - `C-c C-c` — finalize and apply the merge
   - `C-c C-k` — abort the merge
6. After merge: the merged content replaces the buffer, shadow is updated, and a push is triggered.

### 1.9 Local file storage

- Default directory: configurable via `gdocs-directory` (default: `~/org/gdocs/`).
- When importing/opening a Doc for the first time, the org file is saved to `gdocs-directory` with a filename derived from the document title (slugified).
- Per-document override: `gdocs-open` and `gdocs-link` accept an optional file path argument.
- If the user already has an org file and uses `gdocs-create` or `gdocs-link`, the file stays where it is.

### 1.10 Images

- When pushing org -> Google Docs:
  - Local images (`[[file:path/to/image.png]]`) are uploaded to a Google Drive folder (configurable via `gdocs-image-folder-id`, defaults to a `gdocs-images` folder auto-created in the user's Drive root).
  - The image is then embedded in the Google Doc via `InsertInlineImage` with the Drive URL.
  - The Drive file ID is cached locally to avoid re-uploading unchanged images.
- When pulling Google Docs -> org:
  - Inline images are downloaded to a local directory (configurable via `gdocs-image-directory`, defaults to `./images/` relative to the org file).
  - Org links reference the local file path.

---

## Phase 2: Comments and suggestions

### 2.1 Comments system (`gdocs-comments.el`)

#### Side buffer

- `gdocs-comments` — Open/toggle a side buffer showing all comments on the linked Google Doc.
- The comments buffer is displayed in a side window (right side by default).
- Each comment shows:
  - Author name and avatar (if available, as text initials)
  - Timestamp
  - The quoted text the comment is anchored to
  - Comment body
  - Reply thread (indented)
  - Resolved status
- Comments are linked to positions in the org buffer. Clicking a comment in the side buffer jumps to the corresponding position in the org buffer.

#### Comment authoring

- `gdocs-comment-add` — Select a region in the org buffer, then type a comment. Creates a comment anchored to that text in the Google Doc.
- `gdocs-comment-reply` — Reply to the comment at point (in the comments buffer).
- `gdocs-comment-resolve` — Resolve the comment at point.
- `gdocs-comment-delete` — Delete the comment at point.

#### Sync

- Comments are fetched when pulling and when the comments buffer is opened.
- New comments created locally are pushed to the Google Doc immediately (not on buffer save — on comment creation).

### 2.2 Suggesting mode

- `gdocs-suggesting-mode` — Toggle between direct edit and suggesting mode for the current buffer.
- When suggesting mode is active:
  - The modeline shows ` GDocs:suggesting`.
  - On push, changes are sent as suggestions (using the Google Docs API's `InsertText` / `DeleteContentRange` with `suggestedInsertionIds` / `suggestedDeletionIds` fields) rather than direct edits.
- When pulling, existing suggestions from others are displayed:
  - Suggested insertions shown with a distinct face (e.g., green underline).
  - Suggested deletions shown with strikethrough.
  - Each suggestion attributed to its author.
- Commands for handling suggestions:
  - `gdocs-suggestion-accept` — Accept the suggestion at point.
  - `gdocs-suggestion-reject` — Reject the suggestion at point.
  - `gdocs-suggestion-accept-all` — Accept all suggestions.
  - `gdocs-suggestion-reject-all` — Reject all suggestions.

### 2.3 Periodic polling

- When `gdocs-mode` is active, a timer polls the Google Doc for changes periodically.
- Poll interval: configurable via `gdocs-poll-interval` (default: 30 seconds).
- Polling checks the document revision ID (lightweight API call).
- Behavior when remote changes are detected:
  - If the local buffer has no unsaved changes: auto-pull and update the buffer silently. Show a brief message: "Pulled remote changes."
  - If the local buffer has unsaved changes: show a warning in the modeline (` GDocs:conflict`) and notify: "Remote changes detected. Save to push your changes, or run `gdocs-pull` to pull."
- Polling is paused while the buffer is in conflict resolution mode.
- `gdocs-poll-toggle` — Enable/disable polling for the current buffer.

---

## Phase 3: Google Drive browser and export

### 3.1 Google Drive browser (`gdocs-drive.el`)

A dired-like buffer for browsing Google Drive.

#### Interface

- `gdocs-drive` — Open the Drive browser. Prompts for account if multiple.
- The buffer lists files and folders in the current Drive directory.
- Columns: icon/type indicator, file name, owner, last modified, sharing status.
- Files are fetched asynchronously; a loading indicator is shown while fetching.

#### Navigation and keybindings

| Key | Action |
|---|---|
| `RET` | Open file (Google Doc -> org buffer) or enter folder |
| `^` | Go up one directory |
| `g` | Refresh the listing |
| `s` | Search Drive (full-text) |
| `m` | Mark a file |
| `u` | Unmark a file |
| `U` | Unmark all |
| `o` | Open file in browser |
| `i` | Show file info (metadata, permissions, revision history) |
| `d` | Mark for deletion |
| `x` | Execute deletions |
| `C` | Create a new Google Doc |
| `R` | Rename a file |
| `q` | Quit the Drive browser |

#### Filtering

- Only show Google Docs by default (filter by MIME type `application/vnd.google-apps.document`).
- Toggle to show all file types with `T`.
- Toggle to show only files shared with you with `S`.

### 3.2 Org export dispatcher backend (`gdocs-export.el`)

Register as an org export backend so users can use the standard org export dispatcher.

- Export menu key: `g` (so the sequence is `C-c C-e g`).
- Sub-options:
  - `C-c C-e g d` — Export to Google Docs (create new or update existing linked Doc).
  - `C-c C-e g o` — Export and open in browser.
- The export backend:
  1. Parses the buffer with org-element.
  2. Converts to Google Docs API requests.
  3. Creates or updates the Google Doc.
  4. Reports the document URL in the echo area.
- For buffers already linked to a Google Doc, this is equivalent to `gdocs-push`.
- For unlinked buffers, this creates a new Google Doc and links the buffer.

---

## Configuration variables

| Variable | Default | Description |
|---|---|---|
| `gdocs-directory` | `"~/org/gdocs/"` | Default directory for synced org files |
| `gdocs-accounts` | `nil` | Alist of named accounts with optional custom credentials |
| `gdocs-token-directory` | `"~/.emacs.d/gdocs/tokens/"` | Directory for OAuth token storage |
| `gdocs-poll-interval` | `30` | Seconds between polls for remote changes |
| `gdocs-image-directory` | `"./images/"` | Directory for downloaded images (relative to org file) |
| `gdocs-image-folder-id` | `nil` | Google Drive folder ID for uploaded images (auto-created if nil) |
| `gdocs-auto-pull-on-open` | `nil` | Whether to auto-pull when opening a linked org file |
| `gdocs-modeline-lighter` | `" GDocs"` | Modeline lighter text |
| `gdocs-comments-side` | `'right` | Side for the comments buffer (`'left` or `'right`) |
| `gdocs-default-account` | `nil` | Default account name when multiple are configured |

---

## Testing strategy

### Unit tests

- **Format conversion** (`gdocs-convert-test.el`):
  - Test each org element type -> Google Docs JSON conversion and vice versa.
  - Test round-trip fidelity: org -> Docs -> org should produce identical content.
  - Test preservation of org-only constructs through markers.
  - Test preservation of Docs-only constructs through opaque markers.
  - Test edge cases: deeply nested lists, complex tables, mixed formatting.

- **Diff engine** (`gdocs-diff-test.el`):
  - Test that diffs between two org ASTs produce correct batchUpdate request sequences.
  - Test insertion, deletion, modification, and reordering of elements.
  - Test that request ordering is correct (reverse deletions, forward insertions).
  - Test the fallback to full replacement for complex diffs.

- **Auth** (`gdocs-auth-test.el`):
  - Test token refresh logic.
  - Test multi-account token selection.
  - Test token file permission handling.

### Integration tests (with API mocking)

- **Sync engine** (`gdocs-sync-test.el`):
  - Mock the Google Docs/Drive API responses using `plz.el` test utilities or a custom mock layer.
  - Test the full push cycle: edit org -> save -> API calls -> shadow update.
  - Test the full pull cycle: API fetch -> convert -> buffer update.
  - Test conflict detection and resolution flow.
  - Test periodic polling with mocked revision changes.

- **Comments** (`gdocs-comments-test.el`):
  - Test comment fetch and display.
  - Test comment creation, reply, resolution, deletion.
  - Test comment anchoring to org buffer positions.

### Manual testing checklist

- [ ] OAuth flow works end-to-end with a real Google account.
- [ ] Create a Google Doc from an org file with various elements (headings, lists, tables, code blocks, images).
- [ ] Edit the org file and verify changes appear in Google Docs.
- [ ] Edit the Google Doc in the browser and pull changes.
- [ ] Verify conflict resolution when both sides are edited.
- [ ] Test with a shared document (suggesting mode, comments).
- [ ] Test multi-account switching.
- [ ] Test the Drive browser navigation.
- [ ] Test export dispatcher integration.
- [ ] Verify no Emacs blocking during API operations.

---

## Implementation phases summary

### Phase 1: MVP — Core sync
- `gdocs-auth.el`: OAuth flow, token management, multi-account
- `gdocs-api.el`: Docs and Drive API client (async)
- `gdocs-convert.el`: Bidirectional org <-> Docs format conversion
- `gdocs-diff.el`: Incremental diff engine
- `gdocs-sync.el`: Push on save, manual pull, conflict detection
- `gdocs-merge.el`: Side-by-side conflict resolution UI
- `gdocs.el`: User commands, `gdocs-mode` minor mode
- **Deliverable**: A user can open/create/sync Google Docs as org files with full formatting support.

### Phase 2: Comments and suggestions
- `gdocs-comments.el`: Side buffer, comment CRUD, comment sync
- Suggesting mode in `gdocs-sync.el` and `gdocs-diff.el`
- Periodic polling in `gdocs-sync.el`
- **Deliverable**: Full collaboration support — comments, suggestions, and live change awareness.

### Phase 3: Drive browser and export
- `gdocs-drive.el`: Dired-like Google Drive browser
- `gdocs-export.el`: Org export dispatcher backend
- **Deliverable**: Complete integration — browse Drive, discover documents, and export via org's standard mechanisms.
