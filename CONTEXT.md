# Misskey Domain Language

**Instance origin:** The HTTPS scheme, host, and optional non-default port identifying one Misskey-compatible server. It contains no credentials, path, query, or fragment.

**Credential label:** The auth-source login used only to locate one encrypted credential record for an instance. It is not remote identity and need not equal the account's Misskey username.

**Credential:** The atomic pair of a scoped bearer token and the stable remote User ID that token authenticates. A token without its User ID is not a Credential.

**API token:** The secret bearer value within a Credential that authorizes permitted operations.

**Account:** One authenticated identity on one Instance origin, identified by that origin and its stable remote User ID. A Credential label may be replaced without preserving Account identity.

**User:** A Misskey identity that may be local or remote to the selected instance. An Account is the currently authenticated local User on an Instance; other Users do not select credentials.

**Authorization session:** One browser consent attempt that yields one Credential at most once. Each attempt has a fresh, unguessable identity.

**Note:** A social item authored on a Misskey-compatible instance. It has a stable ID and may contain text, files, a poll, a reply target, or a renote target.

**Drive file:** An Account-owned remote file with a stable ID that may be attached to one or more Notes.

**Draft attachment:** A file selected for one Compose body. Before upload it names a local file; after upload it retains the resulting Drive file identity so later publish attempts can reuse it.


**Timeline:** An ordered stream of Notes selected by one visibility and source policy.

**Timeline kind:** One of the basic Home, Local, Social, or Global policies. Home follows the Account's subscriptions; Local contains local-instance Notes; Social combines Home and Local; Global includes visible federated Notes.

**Pure renote:** A note with its own stable identity and author but no original content, whose displayed content is another note.

**Quoted note:** A note with original content that also refers to another note.

**Content warning:** A summary that guards a note body until the reader explicitly reveals it.

**Thread:** A focused Note together with its visible ancestor chain and replies.

**Profile:** The detailed public identity of a User together with that User's queryable Notes and relationships.

**Notification:** An Account-directed event with a stable identity and kind, usually referring to an actor, a Note, or both.

**Notification acknowledgement:** An explicit Account action that marks remote Notifications read. Reading, displaying, refreshing, or paging Notifications is not acknowledgement.


**Reaction:** A User's emoji response to a Note. A Note may expose aggregate reaction counts and the Account's own reaction separately.

**Favorite:** The Account's private saved reference to a Note.

**Compose body:** User-authored note text in one Appkit compose part. It is locked while a publish request is in flight, then becomes editable again and survives a failed or remotely uncertain attempt.

**Compose draft:** One unpublished Misskey compose buffer. It may contain several ordered notes; publishing creates the first note, then each later note as a reply.

**Publish request:** One attempt to create a note. Once dispatched, any failure has an unknown remote outcome because the instance may already have created the note.

**Unknown remote outcome:** A post-dispatch failure where the client cannot prove whether the instance applied the requested write. Retrying is a separate user decision, never an automatic continuation of the failed request.
