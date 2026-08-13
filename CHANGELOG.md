# Changelog

## Unreleased

### Added

- Added a public `M-x misskey-compose` workflow for public plain-text notes on a configured Misskey-compatible HTTPS origin.
- Added auth-source Bearer-token lookup, Plz/curl transport with retries and redirects disabled, bounded JSON response decoding, structured Misskey API errors, Appkit-owned request cancellation, and unknown-outcome reporting for every post-dispatch failure.
- Added first-use MiAuth browser authorization through `M-x misskey-authorize`, `misskey-home`, and `misskey-compose`; scoped tokens are persisted to a configured encrypted auth-source file under a Misskey-specific service label.
- Added an Appkit Compose surface with generated instance, visibility, and request-state fields while keeping the draft body editable and available after failures.
- Compose publish now uses Appkit's shared submit session for in-flight state. `C-c C-k` still refuses while a publish is in flight because Misskey has not attached a transport cancel hook yet.
- A compose draft can hold several ordered notes on Appkit's multi-part surface. `C-c C-n` inserts another note after the current one, `C-c C-p` drops the current extra note, and publish creates the first note then each later note with `replyId`.
- Added `M-x misskey-home` with a clickable header line and `TAB` cycling across Home, Local, Social, and Global in one Appkit view. Each mode retains session-owned canonical state for loaded notes and semantic position, and switching retires the superseded request before replacing the active view state.
- Added `N` pagination for older timeline notes through Misskey's `untilId` contract, with duplicate-boundary removal, exhaustion tracking, and Appkit-preserved point and viewport position.
- Added safe rendering for pure renotes, quoted notes, content warnings, visibility, local-only state, and note counters.
- Added lifecycle-owned asynchronous author avatars to timeline views, with stable placeholder geometry, shared bounded transfers, and a disk cache.
- Added lifecycle-owned inline image and video-thumbnail previews, accessible fallback text, in-Emacs image opening, Appkit video playback, and sensitive-media hiding.
- Bound timeline views, compose drafts, credentials, and Appkit request ownership to the same captured account.
- Added authenticated read requests whose empty arrays remain successful and whose failures are not mislabeled as uncertain writes.

### Fixed

- Compose is now a chatbuf: committed notes render as draft rows, the trailing composer holds the current note, and undo no longer rewrites generated chrome.
