# Changelog

## Unreleased

### Added

- Added a public `M-x misskey-compose` workflow for public plain-text notes on a configured Misskey-compatible HTTPS origin.
- Added auth-source Bearer-token lookup, bounded JSON response decoding, structured Misskey API errors, Appkit-owned request cancellation, and unknown-outcome reporting for every post-dispatch failure.
- Added an Appkit Compose surface with generated instance, visibility, and request-state fields while keeping the draft body editable and available after failures.
