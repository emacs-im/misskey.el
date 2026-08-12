# misskey.el

`misskey.el` is an Emacs client for Misskey-compatible servers. It reads an authenticated home timeline through Appkit-backed keyed views and publishes public plain-text notes from an Appkit Compose editor.

## Installation

Make `misskey.el`, Appkit, and Plz available on `load-path`, then load the public entry point:

```elisp
(require 'misskey)
```

The package requires Emacs 29.1, Appkit 0.2.4, Plz 0.9.1, and the `curl` executable.

## Configuration

Select an instance and the auth-source login naming that account:

```elisp
(setq misskey-instance-url "https://example.social"
      misskey-auth-source-user "alice")
```

The instance must be an HTTPS origin without a path, query, fragment, or credentials. Store the API token under the same host and login. For example, an `~/.authinfo.gpg` entry is:

```text
machine example.social login alice password YOUR_API_TOKEN
```

The token needs `read:account` for the home timeline and `write:notes` for publishing. It is read only when a request starts, sent in an `Authorization: Bearer` header, omitted from JSON bodies, and redacted from locally surfaced setup errors.

## Quick Start

Open the authenticated home timeline:

```text
M-x misskey-home
```

Timeline keys are:

- `g`: refresh
- `n` / `p`: move between notes using Appkit discussion navigation
- `RET`: reveal or hide content guarded by a content warning
- `c`: compose for the timeline's captured account

Run `M-x misskey-compose` to open the composer directly. Enter the note body, then use:

- `C-c C-c`: publish the draft. A multi-note draft creates the first note, then each later note as a reply
- `C-c C-n`: insert another note after the current one
- `C-c C-p`: drop the current extra note
- `C-c C-k`: cancel the draft; this is refused while a publish is in flight

The composer currently publishes only non-empty plain text with `visibility` set to `public`.

## Home Timeline

The timeline uses the common Misskey and Sharkey `notes/timeline` contract. `misskey-timeline-limit` controls the number of notes requested, from 1 through 100. Refreshes reconcile rows by note ID and preserve semantic point and viewport position through Appkit.

Text, pure renotes, quoted notes, visibility, local-only state, counts, and attachment counts are rendered. Content-warning bodies remain hidden until explicitly revealed. MFM interpretation, media previews, pagination, replies, reactions, and renote actions are intentionally deferred to later vertical slices.

## Write Safety

A `notes/create` request is dispatched once through Plz and a fresh `curl` process. The transport profile ignores curl configuration files and disables redirects and retries; the Bearer token reaches curl through Plz's standard-input configuration rather than process arguments. The request becomes owned by the captured account's Appkit session before control returns to the user. The submitted body is locked while that request is in flight, preventing edits that were not part of the dispatched note. After dispatch, transport, cancellation, HTTP, malformed-response, and remote API failures are all reported as an unknown remote outcome because the server may already have created the note. The compose buffer and body stay available and editable again on failure so the user can inspect the server before deciding whether to try again.
