# Changelog

## Unreleased

### Added

- Added MiAuth authorization through encrypted auth-source storage for `read:account`, `read:notifications`, `write:notes`, `write:reactions`, `write:favorites`, `write:following`, `write:notifications`, and `write:drive`; bearer token and stable remote user ID are persisted as one credential, and `M-x misskey-authorize` atomically replaces both and their live session.
- Added Appkit-backed Home, Local, Social, and Global timelines with retained per-mode state, stable-key reconciliation, semantic position preservation, older-page cursors, content-warning reveal state, and shared media resources.
- Added protocol-neutral Misskey Note validation and rendering shared by timelines, threads, profiles, and searches, including pure renotes, quotes, visibility, local-only state, counts, replies, media, and stable presentation dependencies.
- Added thread views with a focused note, ancestor chain, direct-reply pagination, Appkit discussion geometry, reply and quote composition, and request replacement.
- Added Appkit Compose drafts with Public, Home, and Followers visibility, ordered reply-chain notes, reply and quote targets, per-part local attachments, streaming Drive uploads, file-only notes, and reuse of successful Drive IDs after later failures.
- Added profile views with Notes, Notes + replies, and Media modes plus paged followers and following directories keyed by user identity.
- Added independent paged note searches whose request, result, cursor, reveal, and lifecycle state is isolated from every other view.
- Added notifications with stable identities, explicit `markAsRead: false` reads, older-page loading, activation of referenced notes or users, and `M` as the only remote mark-all-read action.
- Added one contextual action path for reactions, favorites, pure renotes, note deletion, follow, and unfollow, with account-scoped state overrides and dependency-driven invalidation across every live view.
- Added lifecycle-owned presentation for every Drive attachment through Appkit, including guarded sensitive media, original-resource open actions, and MIME-filtered avatar/image/video preview caching.
- Migrated Misskey sessions and generated hosts to canonical Appkit Apps, Surfaces, and Effects. Media acquisition now commits before presentation, rejects superseded or closed-host results, and isolates disk caches by account while retaining targeted projection updates and semantic positions.
- Added authenticated JSON and streaming multipart curl transports with redirects and retries disabled, raw response and diagnostic byte caps, strict bearer validation, Appkit-owned cancellation, token redaction, and unknown-outcome reporting for every post-dispatch write failure.
- Added Drive upload progress on the compose submit status strip, measured from curl's upload meter without loading the file into Emacs.

### Fixed

- Compose uses Appkit's chatbuf surface: generated context, status, attachment rows, and committed parts no longer share the editable input region.
- Persisting completed uploads and confirmed thread parts no longer cancels the active publication operation.
- Successful Drive uploads remain attached to a failed draft, avoiding duplicate uploads on a deliberate retry of note creation.
- Browsing or paging a notification view never implicitly acknowledges remote notifications.
- Note views keep timestamps at the live window's right edge, elide long author headings in narrow windows, and restore the complete heading after widening.
- Pure Renotes show `renoted by …` as pre-heading social context, followed by the original author's avatar, heading, timestamp, and content.
