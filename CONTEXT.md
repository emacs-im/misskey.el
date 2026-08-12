# Misskey Domain Language

**Instance origin:** The HTTPS scheme, host, and optional non-default port identifying one Misskey-compatible server. It contains no credentials, path, query, or fragment.

**Credential label:** The auth-source login used to distinguish locally stored tokens for one instance. It need not equal the account's Misskey username.

**API token:** An auth-source secret issued by an instance with the permissions needed for requested operations.

**Account:** One identity on one instance. Its instance origin and credential label select the API token authorizing operations for that identity.

**Authorization session:** One browser consent attempt that grants a new API token with selected permissions. Each attempt has a fresh, unguessable identity and yields its token at most once.

**Note:** A social item authored on a Misskey-compatible instance. It has a stable ID and may contain text, files, a poll, a reply target, or a renote target.

**Home timeline:** The ordered notes visible to an account from itself, followed users, and followed channels according to the instance's policy.

**Pure renote:** A note with its own stable identity and author but no original content, whose displayed content is another note.

**Quoted note:** A note with original content that also refers to another note.

**Content warning:** A summary that guards a note body until the reader explicitly reveals it.

**Compose body:** User-authored note text in one Appkit compose part. It is locked while a publish request is in flight, then becomes editable again and survives a failed or remotely uncertain attempt.

**Compose draft:** One unpublished Misskey compose buffer. It may contain several ordered notes; publishing creates the first note, then each later note as a reply.

**Publish request:** One attempt to create a note. Once dispatched, any failure has an unknown remote outcome because the instance may already have created the note.
