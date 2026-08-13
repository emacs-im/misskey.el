# misskey.el

`misskey.el` is an Appkit-backed Emacs client for Misskey-compatible servers. It provides timelines, threads, user profiles and relationships, independent note searches, notifications with explicit read acknowledgement, media previews, multi-note composition with Drive attachments, and contextual note and user actions.

## Installation

Make `misskey.el`, Appkit, and Plz available on `load-path`, then load the public entry point:

```elisp
(require 'misskey)
```

The package requires Emacs 29.1, Appkit 0.2.14, Plz 0.9.1, and the `curl` and `uuidgen` executables.

## Configuration and authorization

Select one HTTPS instance origin and a local credential label:

```elisp
(setq misskey-instance-url "https://example.social"
      misskey-auth-source-user "alice")
```

The origin cannot contain credentials, a path, a query, or a fragment. The credential label locates one encrypted auth-source record and need not equal the Misskey username. Account and session identity instead use the instance origin plus the stable remote user ID returned by Misskey.

Run `M-x misskey-authorize` to open a fresh MiAuth consent session and atomically replace the configured credential. First use of the home timeline or composer also authorizes when no credential exists. The bearer token and remote user ID are saved together through the first configured encrypted netrc/authinfo source ending in `.gpg`; authorization refuses to write a plain source. Explicit reauthorization stops and replaces any live session for the previous credential.

Token-only records created by older versions are deliberately rejected with a reauthorization message because they cannot establish stable account identity. Run `M-x misskey-authorize` rather than editing the stored record by hand.

The requested scopes are `read:account`, `read:notifications`, `write:notes`, `write:reactions`, `write:favorites`, `write:following`, `write:notifications`, and `write:drive`. Existing credentials created with fewer scopes do not gain scopes automatically; run `M-x misskey-authorize` again and approve the new permission set.

An instance on a non-default HTTPS port uses `misskey-PORT`, such as `misskey-8443`, as the auth-source service label.

## Quick Start

- `M-x misskey-home`: open the authenticated Home timeline.
- `M-x misskey-thread`: open a thread by note ID.
- `M-x misskey-profile-open`: open a user by stable ID or `@username[@host]`.
- `M-x misskey-search`: open a fresh, independently paged note search.
- `M-x misskey-notifications`: open account notifications without marking them read.
- `M-x misskey-compose`: open a new compose draft.

Shared note-view keys are:

- `g`: refresh.
- `n` / `p`: move between notes.
- `N`: load one older page while preserving semantic position.
- `RET`: reveal or hide a content-warning body.
- `t`: open the thread at point.
- `r` / `q`: compose a reply or quote.
- `a`: prefix contextual note and user actions.

Timeline `TAB` cycles Home, Local, Social, and Global in one view while retaining each mode's loaded notes and position. Profile `TAB` cycles Notes, Notes + replies, and Media; `f` and `F` open followers and following directories. Each invocation of `misskey-search` owns an independent result set, cursor, request, and content-warning state.

## Media

Graphical note views fetch author avatars plus MIME-qualified image and video thumbnails asynchronously through Appkit's bounded resource store and task queue. Every Drive attachment remains visible and actionable; audio, PDF, archives, and other generic files open their original HTTPS resource through Appkit's lifecycle-owned cached-file workflow. Text renders first, placeholders preserve row geometry, successful responses share a disk cache, and failed resources remain retryable across views. Sensitive media stays hidden until the user explicitly reveals its note, whether or not the note has a content warning.

Set `misskey-timeline-show-avatars` or `misskey-timeline-show-media` to `nil` to disable those transfers. `misskey-timeline-media-preview-width` and `misskey-timeline-media-preview-height` bound inline previews.

## Compose and Drive attachments

The Appkit Compose surface keeps generated context and status outside the editable body. Commands are:

- `C-c C-c`: publish the draft once.
- `C-c C-v`: choose Public, Home, or Followers visibility.
- `C-c C-a`: attach a readable local file to the current note.
- `C-c C-d`: detach one file from the current note.
- `C-c C-n`: insert another note after the current part.
- `C-c C-p`: remove the current extra note.
- `C-c C-k`: abandon an idle draft; an in-flight write cannot be silently canceled.

Each note accepts up to 16 files. File-only notes are supported. Local files stream to `drive/files/create` without entering Emacs memory, and `notes/create` receives their Drive IDs. The compose State field shows a progress bar and percent for the file currently uploading. A multi-note draft publishes in order and makes each later note a reply to the preceding created note.

After a successful upload, its Drive ID is stored in that compose item's Appkit metadata. If later note creation fails, the draft remains editable and a deliberate retry reuses that Drive file instead of uploading it again.

## Note and user actions

Press `a` followed by:

- `r` / `R`: add a reaction string or remove the account's current reaction.
- `f` / `F`: favorite or unfavorite the note.
- `n`: create a pure renote.
- `d`: confirm and delete the exact note row at point.
- `+` / `-`: follow or unfollow the user at point.

Successful actions install account-scoped overrides and invalidate matching note or user resources in every live Appkit view. Reaction, renote, favorite, follow, and deletion state therefore updates without mutating cached server payloads or maintaining per-view copies.

## Notifications

Opening, refreshing, and paging notifications always send `markAsRead: false`. `M` is the only command that calls `notifications/mark-all-as-read`; merely browsing a read-only notification buffer never acknowledges remote state. `RET` opens the notification's note or actor when one is present.

## Write safety

Every authenticated JSON request and Drive upload uses one bounded streaming curl dispatch. Both profiles ignore curl configuration files, reject redirects, disable retries, strictly validate bearer credentials, keep the bearer token out of request bodies and process arguments, and cancel immediately when raw response bytes exceed the limit. Appkit owns each request before control returns to the user and cancels remaining work when the account session stops.

After dispatch, transport, cancellation, HTTP, malformed-response, and remote API failures are reported as an unknown remote outcome because the instance may already have applied the write. `misskey.el` never retries a Misskey write automatically. The affected compose draft remains available after failure so the user can inspect the server before deciding whether to try again.
