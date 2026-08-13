# Misskey Domain Language

**Instance origin:** The HTTPS scheme, host, and optional non-default port identifying one Misskey-compatible server. It contains no credentials, path, query, or fragment.

**Credential label:** The auth-source login used to distinguish locally stored tokens for one instance. It need not equal the account's Misskey username.

**API token:** An auth-source secret issued by an instance with the permissions needed for requested operations.

**Account:** One identity on one instance. Its instance origin and credential label select the API token authorizing operations for that identity.

**User:** A Misskey identity that may be local or remote to the selected instance. Unlike an Account, a User need not be authenticated and does not select credentials.

**Authorization session:** One browser consent attempt that grants a new API token with selected permissions. Each attempt has a fresh, unguessable identity and yields its token at most once.

**Note:** A social item authored on a Misskey-compatible instance. It has a stable ID and may contain text, files, a poll, a reply target, or a renote target.

**Timeline:** An ordered stream of Notes selected by one visibility and source policy.

**Timeline kind:** One of the basic Home, Local, Social, or Global policies. Home follows the Account's subscriptions; Local contains local-instance Notes; Social combines Home and Local; Global includes visible federated Notes.

**Pure renote:** A note with its own stable identity and author but no original content, whose displayed content is another note.

**Quoted note:** A note with original content that also refers to another note.

**Content warning:** A summary that guards a note body until the reader explicitly reveals it.

**Thread:** A focused Note together with its visible ancestor chain and replies.

**Profile:** The detailed public identity of a User together with that User's queryable Notes and relationships.

**Notification:** An Account-directed event with a stable identity and kind, usually referring to an actor, a Note, or both.

**Reaction:** A User's emoji response to a Note. A Note may expose aggregate reaction counts and the Account's own reaction separately.

**Favorite:** The Account's private saved reference to a Note.

**Compose body:** User-authored note text in one Appkit compose part. It is locked while a publish request is in flight, then becomes editable again and survives a failed or remotely uncertain attempt.

**Compose draft:** One unpublished Misskey compose buffer. It may contain several ordered notes; publishing creates the first note, then each later note as a reply.

**Publish request:** One attempt to create a note. Once dispatched, any failure has an unknown remote outcome because the instance may already have created the note.
