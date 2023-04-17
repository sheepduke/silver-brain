CREATE TABLE IF NOT EXISTS [Concept] (
    [Uuid] TEXT PRIMARY KEY,
    [Name] TEXT NOT NULL,
    [CreatedAt] TEXT NOT NULL,
    [UpdatedAt] TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS [ConceptAlias] (
    [Id] INTEGER PRIMARY KEY,
    [ConceptUuid] TEXT NOT NULL,
    [Alias] TEXT NOT NULL,
    FOREIGN KEY (ConceptUuid) REFERENCES [Concept] (Uuid)
);

CREATE TABLE if not exists [ConceptAttachment] (
    [Id] INTEGER PRIMARY KEY,
    [ConceptUuid] TEXT NOT NULL,
    [Name] TEXT NOT NULL,
    [ContentType] TEXT NOT NULL,
    [ContentLength] INTEGER NOT NULL,
    FOREIGN KEY (ConceptUuid) REFERENCES [Concept] (Uuid)
);
 
CREATE TABLE IF NOT EXISTS [ConceptRelationPair] (
    [Id] INTEGER PRIMARY KEY,
    [ConceptUuid] TEXT NOT NULL,
    [OtherUuid] TEXT NOT NULL,
    UNIQUE (ConceptUuid, OtherUuid),
    FOREIGN KEY (ConceptUuid) REFERENCES [Concept] (Uuid),
    FOREIGN KEY (OtherUuid) REFERENCES [Concept] (Uuid)
);

CREATE TABLE IF NOT EXISTS [ConceptLink] (
    [Id] INTEGER PRIMARY KEY,
    [SourceUuid] TEXT NOT NULL,
    [RelationUuid] TEXT NOT NULL,
    [TargetUuid] TEXT NOT NULL
);