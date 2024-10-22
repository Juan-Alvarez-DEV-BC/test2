erDiagram
    CLIENTES {
        BIGINT id PK "PRIMARY KEY GENERATED ALWAYS AS IDENTITY"
        TEXT nombre "NOT NULL"
        TEXT email "NOT NULL UNIQUE"
        TEXT telefono
        TEXT direccion
    }
    
    PRESTAMOS {
        BIGINT id PK "PRIMARY KEY GENERATED ALWAYS AS IDENTITY"
        NUMERIC monto "NOT NULL"
        NUMERIC interes "NOT NULL"
        INT duracion_meses "NOT NULL"
        TEXT estado "CHECK (estado IN ('Pendiente', 'Aprobado', 'Rechazado')) NOT NULL"
        BIGINT cliente_id "REFERENCES clientes(id)"
    }

    CLIENTES ||--o{ PRESTAMOS : "cliente_id"
