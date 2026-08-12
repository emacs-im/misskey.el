# Misskey Domain Language

**Instance origin:** The HTTPS scheme, host, and optional non-default port identifying one Misskey-compatible server. It contains no credentials, path, query, or fragment.

**API token:** An auth-source secret issued by an instance with the permissions needed for requested operations.

**Note:** A social item created by the Misskey `notes/create` operation. In the first supported workflow it contains non-empty plain text and public visibility.

**Compose body:** User-authored note text. It is locked while one publish request is in flight, then becomes editable again and survives a failed or remotely uncertain attempt.

**Publish request:** One attempt to create a note. Once dispatched, any failure has an unknown remote outcome because the instance may already have created the note.
