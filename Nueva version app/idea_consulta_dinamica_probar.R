library(RODBC)

# Conexión a tu base de datos
conn <- odbcConnect("tu_basededatos")

# Definir el nombre de la tabla temporal
tableName <- "#TempTable"

# Consulta SQL dinámica
query <- "
DECLARE @columnName NVARCHAR(MAX);
DECLARE @sqlQuery NVARCHAR(MAX);

-- Consulta para obtener las columnas de tipo VARCHAR de la tabla temporal
SET @sqlQuery = '
    SELECT COLUMN_NAME
    FROM information_schema.columns
    WHERE TABLE_NAME = ? 
    AND DATA_TYPE = ''varchar''
';

-- Ejecutar la consulta para obtener las columnas de tipo VARCHAR
EXEC sp_executesql @sqlQuery, N'@tableName NVARCHAR(MAX)', ?;

-- Iterar sobre las columnas obtenidas y reemplazar ";" por "," en los valores de esas columnas
DECLARE columnCursor CURSOR FOR
    SELECT COLUMN_NAME
    FROM information_schema.columns
    WHERE TABLE_NAME = ?
    AND DATA_TYPE = 'varchar';

OPEN columnCursor;
FETCH NEXT FROM columnCursor INTO @columnName;

WHILE @@FETCH_STATUS = 0
BEGIN
    SET @sqlQuery = '
        UPDATE ' + ? + '
        SET ' + @columnName + ' = REPLACE(' + @columnName + ', '';'', '','')
    ';

    EXEC sp_executesql @sqlQuery;

    FETCH NEXT FROM columnCursor INTO @columnName;
END

CLOSE columnCursor;
DEALLOCATE columnCursor;

-- Consulta para obtener los datos actualizados de la tabla temporal
SELECT * FROM ?
"

# Ejecutar la consulta y almacenar los resultados en un dataframe
resultados <- sqlQuery(conn, query, tableName, tableName, tableName, tableName, tableName)

# Cerrar la conexión a la base de datos
close(conn)

# Visualizar los resultados
print(resultados)
