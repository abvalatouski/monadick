CREATE TABLE IF NOT EXISTS notes (
    id   INT  NOT NULL GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    note TEXT NOT NULL
);
