CREATE TABLE IF NOT EXISTS Concept (
    Uuid TEXT PRIMARY KEY,
    Name TEXT NOT NULL,
    Summary TEXT NOT NULL,
    ContentType TEXT NOT NULL,
    Content TEXT NOT NULL,
    CreatedAt TEXT NOT NULL,
    UpdatedAt TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS ConceptAlias (
    Id INTEGER PRIMARY KEY,
    ConceptUuid TEXT NOT NULL,
    Alias TEXT NOT NULL,
    FOREIGN KEY (ConceptUuid) REFERENCES Concept (Uuid)
);

CREATE TABLE if NOT EXISTS Attachment (
    Id INTEGER PRIMARY KEY,
    Name TEXT NOT NULL,
    FilePath TEXT NOT NULL
);

CREATE TABLE if NOT EXISTS ConceptAttachment (
    AttachmentId INTEGER NOT NULL,
    ConceptUuid TEXT NOT NULL,
    PRIMARY KEY (AttachmentId, ConceptUuid),
    FOREIGN KEY (AttachmentId) REFERENCES Attachment (Id),
    FOREIGN KEY (ConceptUuid) REFERENCES Concept (Uuid)
);

CREATE TABLE IF NOT EXISTS ConceptRelation (
    ConceptUuid TEXT PRIMARY KEY,
    FOREIGN KEY (ConceptUuid) REFERENCES Concept (Uuid)
);

CREATE TABLE IF NOT EXISTS ConceptLink (
    Id INTEGER PRIMARY KEY,
    SourceUuid TEXT NOT NULL,
    RelationUuid TEXT NOT NULL,
    TargetUuid TEXT NOT NULL
);
