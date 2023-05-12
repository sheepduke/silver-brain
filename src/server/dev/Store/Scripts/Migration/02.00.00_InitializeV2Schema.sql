CREATE TABLE IF NOT EXISTS Concept (
    Id TEXT PRIMARY KEY,
    Name TEXT NOT NULL,
    Summary TEXT NOT NULL,
    ContentType TEXT NOT NULL,
    Content TEXT NOT NULL,
    CreatedAt TEXT NOT NULL,
    UpdatedAt TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS ConceptAlias (
    Id TEXT PRIMARY KEY,
    ConceptId TEXT NOT NULL,
    Alias TEXT NOT NULL,
    FOREIGN KEY (ConceptId) REFERENCES Concept (Id)
) WITHOUT ROWID;

CREATE TABLE if NOT EXISTS Attachment (
    Id TEXT PRIMARY KEY,
    Name TEXT NOT NULL,
    FilePath TEXT NOT NULL
) WITHOUT ROWID;

CREATE TABLE if NOT EXISTS ConceptAttachment (
    AttachmentId TEXT NOT NULL,
    ConceptId TEXT NOT NULL,
    PRIMARY KEY (AttachmentId, ConceptId),
    FOREIGN KEY (AttachmentId) REFERENCES Attachment (Id),
    FOREIGN KEY (ConceptId) REFERENCES Concept (Id)
) WITHOUT ROWID;

CREATE TABLE IF NOT EXISTS ConceptLink (
    Id INTEGER PRIMARY KEY,
    SourceId TEXT NOT NULL,
    RelationId TEXT NOT NULL,
    TargetId TEXT NOT NULL
) WITHOUT ROWID;

CREATE TABLE IF NOT EXISTS ConceptPropertyIsRelation(
    ConceptId TEXT PRIMARY KEY,
    FOREIGN KEY (ConceptId) REFERENCES Concept (Id)
) WITHOUT ROWID;
