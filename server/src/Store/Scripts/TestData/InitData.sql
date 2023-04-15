-- Concept
INSERT INTO [Concept] (Uuid, Name, CreatedAt, UpdatedAt)
    VALUES ('0001', 'Editor', '2023-04-13 00:50:00Z', '2023-04-13 00:55:10Z');
INSERT INTO [Concept] (Uuid, Name, CreatedAt, UpdatedAt)
    VALUES ('0002', 'Emacs', '2023-04-13 00:50:00Z', '2023-04-13 00:55:10Z');
INSERT INTO [Concept] (Uuid, Name, CreatedAt, UpdatedAt)
    VALUES ('0003', 'Vim', '2023-04-13 00:50:00Z', '2023-04-13 00:55:10Z');
INSERT INTO [Concept] (Uuid, Name, CreatedAt, UpdatedAt)
    VALUES ('0010', 'Kubernates', '2023-04-13 00:50:00Z', '2023-04-13 00:55:10Z');
INSERT INTO [Concept] (Uuid, Name, CreatedAt, UpdatedAt)
    VALUES ('0011', 'Docker', '2023-04-13 00:50:00Z', '2023-04-13 00:55:10Z');
INSERT INTO [Concept] (Uuid, Name, CreatedAt, UpdatedAt)
    VALUES ('0012', 'Docker File', '2023-04-13 00:50:00Z', '2023-04-13 00:55:10Z');
INSERT INTO [Concept] (Uuid, Name, CreatedAt, UpdatedAt)
    VALUES ('0013', 'Configuration File', '2023-04-13 00:50:00Z', '2023-04-13 00:55:10Z');
INSERT INTO [Concept] (Uuid, Name, CreatedAt, UpdatedAt)
    VALUES ('1001', 'Is a', '2023-04-13 00:50:00Z', '2023-04-13 00:55:10Z');
INSERT INTO [Concept] (Uuid, Name, CreatedAt, UpdatedAt)
    VALUES ('1002', 'Contains', '2023-04-13 00:50:00Z', '2023-04-13 00:55:10Z');
INSERT INTO [Concept] (Uuid, Name, CreatedAt, UpdatedAt)
    VALUES ('1003', 'Relates to', '2023-04-13 00:50:00Z', '2023-04-13 00:55:10Z');
INSERT INTO [Concept] (Uuid, Name, CreatedAt, UpdatedAt)
    VALUES ('1004', 'Supports', '2023-04-13 00:50:00Z', '2023-04-13 00:55:10Z');
INSERT INTO [Concept] (Uuid, Name, CreatedAt, UpdatedAt)
    VALUES ('1005', 'Is Supported by', '2023-04-13 00:50:00Z', '2023-04-13 00:55:10Z');

-- ConceptAlias
INSERT INTO [ConceptAlias] (ConceptUuid, Alias) VALUES ('0002', 'Editor of gods');
INSERT INTO [ConceptAlias] (ConceptUuid, Alias) VALUES ('0003', 'God of editors');
INSERT INTO [ConceptAlias] (ConceptUuid, Alias) VALUES ('0010', 'K8s');

-- ConceptAttachment
INSERT INTO [ConceptAttachment] (ConceptUuid, Name, ContentType, ContentLength)
    VALUES ('0002', 'Introduction', 'text/org', 21);
INSERT INTO [ConceptAttachment] (ConceptUuid, Name, ContentType, ContentLength)
    VALUES ('0003', 'Body', 'text/md', 13);
INSERT INTO [ConceptAttachment] (ConceptUuid, Name, ContentType, ContentLength)
    VALUES ('0003', 'Second', 'text/plain', 15);

-- ConceptRelationPair
INSERT INTO [ConceptRelationPair] (ConceptUuid, OtherUuid) VALUES ('1001', '1002');
INSERT INTO [ConceptRelationPair] (ConceptUuid, OtherUuid) VALUES ('1003', '1003');
INSERT INTO [ConceptRelationPair] (ConceptUuid, OtherUuid) VALUES ('1004', '1005');

-- ConceptLink
INSERT INTO [ConceptLink] (SourceUuid, RelationUuid, TargetUuid)
    VALUES ('0002', '1001', '0001');
INSERT INTO [ConceptLink] (SourceUuid, RelationUuid, TargetUuid)
    VALUES ('0001', '1002', '0003');
INSERT INTO [ConceptLink] (SourceUuid, RelationUuid, TargetUuid)
    VALUES ('0002', '1003', '0003');
INSERT INTO [ConceptLink] (SourceUuid, RelationUuid, TargetUuid)
    VALUES ('0011', '1005', '0010');
INSERT INTO [ConceptLink] (SourceUuid, RelationUuid, TargetUuid)
    VALUES ('0011', '1002', '0012');
INSERT INTO [ConceptLink] (SourceUuid, RelationUuid, TargetUuid)
    VALUES ('0003', '1004', '0012');
