# Changelog

## Unreleased

### Added

- Added a public `M-x misskey-compose` workflow for public plain-text notes on a configured Misskey-compatible HTTPS origin.
- Added auth-source Bearer-token lookup, bounded JSON response decoding, structured Misskey API errors, Appkit-owned request cancellation, and unknown-outcome reporting for every post-dispatch failure.
- Added an Appkit Compose surface with generated instance, visibility, and request-state fields while keeping the draft body editable and available after failures.
- Added `M-x misskey-home`, backed by Appkit views, keyed projection reconciliation, and discussion rows for the authenticated home timeline.
- Added safe rendering for pure renotes, quoted notes, content warnings, visibility, local-only state, and note counters.
- Bound timeline views, compose drafts, credentials, and Appkit request ownership to the same captured account.
- Added authenticated read requests whose empty arrays remain successful and whose failures are not mislabeled as uncertain writes.
