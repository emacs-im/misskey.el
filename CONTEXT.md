# Misskey Domain Language

**Instance origin:** The HTTPS scheme, host, and optional non-default port identifying one Misskey-compatible server. It contains no credentials, path, query, or fragment.

**API token:** An auth-source secret issued by an instance with the permissions needed for requested operations.

**Account:** One identity on one instance, selected by an instance origin and an auth-source login. Its API token authorizes operations for that identity.

**Note:** A social item authored on a Misskey-compatible instance. It has a stable ID and may contain text, files, a poll, a reply target, or a renote target.

**Home timeline:** The ordered notes visible to an account from itself, followed users, and followed channels according to the instance's policy.

**Pure renote:** A note with its own stable identity and author but no original content, whose displayed content is another note.

**Quoted note:** A note with original content that also refers to another note.

**Content warning:** A summary that guards a note body until the reader explicitly reveals it.

**Compose body:** User-authored note text. It is locked while one publish request is in flight, then becomes editable again and survives a failed or remotely uncertain attempt.

**Publish request:** One attempt to create a note. Once dispatched, any failure has an unknown remote outcome because the instance may already have created the note.
