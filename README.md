# `gdocs`: Google Docs integration for Emacs org-mode

`gdocs` provides bidirectional synchronization between org-mode files and Google Docs. Open, edit, create, and push Google Docs entirely from within Emacs, using org-mode as the native editing format.

## Overview

The package addresses three main workflows:

- **Publish org to Google Docs.** Create a Google Doc from an existing org file to share with collaborators. The two files remain in sync: edits to either propagate to the other.
- **Read and edit shared Google Docs.** Open a Google Doc shared by someone else as an org-mode buffer in Emacs. Edit without leaving Emacs.
- **Import existing Google Docs.** Import old Google Docs into org files. The files remain synchronized, but you may later choose to unlink or delete the Google Doc.

Under the hood, `gdocs` handles OAuth 2.0 authentication with multi-account support, bidirectional format conversion through an intermediate representation, incremental diffing via a longest-common-subsequence algorithm so only changes are sent over the wire, and a side-by-side conflict resolution UI when local and remote edits collide. All API operations are fully asynchronous via `plz.el`, so Emacs never blocks during network requests.

The `gdocs-mode` minor mode activates automatically on linked buffers, providing auto-push on save, a modeline sync indicator, and keybindings under the `C-c g` prefix for push, pull, status, and browser access.

## Installation

`gdocs` requires Emacs 29.1 or later.

### package-vc (built-in since Emacs 30)

```emacs-lisp
(package-vc-install "https://github.com/benthamite/gdocs")
```

### Elpaca

```emacs-lisp
(use-package gdocs
  :ensure (gdocs :host github :repo "benthamite/gdocs"))
```

### straight.el

```emacs-lisp
(straight-use-package
 '(gdocs :type git :host github :repo "benthamite/gdocs"))
```

### Dependencies

- [plz](https://github.com/alphapapa/plz.el) (0.7+) — asynchronous HTTP requests
- [dash](https://github.com/magnars/dash.el) (2.19+) — list manipulation utilities
- [s](https://github.com/magnars/s.el) (1.13+) — string manipulation utilities
- org (9.6+) — built into Emacs

## Quick start

1. Obtain OAuth credentials from the [Google Cloud Console](https://console.cloud.google.com/) (see the [manual](README.org) for a detailed walkthrough).

2. Configure your credentials:

   ```emacs-lisp
   (setq gdocs-accounts
         '(("personal" . ((client-id . "YOUR-CLIENT-ID")
                          (client-secret . "YOUR-CLIENT-SECRET")))))
   ```

3. Authenticate: `M-x gdocs-authenticate`.

4. Open an existing Google Doc: `M-x gdocs-open`.

5. Or create a new Google Doc from an org file: `M-x gdocs-create`.

Once a buffer is linked, changes are pushed to Google Docs on every save (when `gdocs-auto-push-on-save` is non-nil), and you can pull remote changes at any time with `M-x gdocs-pull`.

## Documentation

For a comprehensive description of all user options, commands, and functions, see the [manual](README.org).

## Roadmap

### Phase 2: comments and suggestions

- [ ] Comments side buffer for viewing and managing Google Docs comments (add, reply, resolve, delete)
- [ ] Suggesting mode — toggle between direct editing and suggestions, with changes sent as Google Docs suggestions rather than direct edits
- [ ] Accept/reject suggestions from other collaborators
- [ ] Periodic polling for remote changes with configurable interval

### Phase 3: Drive browser and export

- [ ] Google Drive browser — a dired-like buffer for browsing, searching, and managing files in Google Drive
- [ ] Org export backend (`C-c C-e g d`) — export to Google Docs via the standard org export dispatcher

## License

[GPL-3.0](LICENSE)
