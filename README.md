# misskey.el

`misskey.el` is an Emacs client for people using Misskey-compatible servers. The current vertical slice opens an Appkit-backed standalone editor and publishes a public plain-text note through the native Misskey `notes/create` API.

## Installation

Make `misskey.el` and Appkit available on `load-path`, then load the public entry point:

```elisp
(require 'misskey)
```

The package requires Emacs 29.1 and Appkit 0.2.4 or newer.

## Configuration

Set the instance to its HTTPS origin only:

```elisp
(setq misskey-instance-url "https://example.social")
```

Store an API token in auth-source under the instance host and the user name `misskey.el`. For example, an `~/.authinfo.gpg` entry is:

```text
machine example.social login misskey.el password YOUR_API_TOKEN
```

The token needs the server's `write:notes` permission. It is read only when a request starts, sent in an `Authorization: Bearer` header, omitted from the JSON body, and redacted from locally surfaced setup errors.

## Quick Start

Run:

```text
M-x misskey-compose
```

Enter the note body, then use:

- `C-c C-c`: publish the note
- `C-c C-k`: cancel the draft before publishing starts

The first slice intentionally publishes only non-empty plain text with `visibility` set to `public`. It does not yet implement timelines, replies, renotes, content warnings, media, alternate visibility, local-only notes, polls, scheduling, or persisted drafts.

## Write Safety

A `notes/create` request is dispatched once through `url.el`: redirects, connection reuse, and `url.el`'s expired-connection replay path are disabled for the write. The request becomes owned by the Misskey Appkit session before control returns to the user. The submitted body is locked while that request is in flight, preventing edits that were not part of the dispatched note. After dispatch, transport, cancellation, HTTP, malformed-response, and remote API failures are all reported as an unknown remote outcome because the server may already have created the note. The compose buffer and body stay available and editable again on failure so the user can inspect the server before deciding whether to try again.
