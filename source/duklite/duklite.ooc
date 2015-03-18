
use sqlite3
import sqlite3/sqlite3

use duktape
import duk/tape

DDatabase: class {

    db: Database 

    init: func (path: String) {
        db = Database new(path)
    }

    exec: func (duk: DukContext) -> Int {
        query := duk requireString(0)
        "query = #{query toString()}" println()
        stmt := db prepare(query toString())

        bindIndex := 1
        numArgs := duk getTop() - 1
        "numArgs = #{numArgs}" println()
        for (argIndex in 1..numArgs) {
            match {
                case duk isNumber(argIndex) =>
                    val := duk requireNumber(argIndex)
                    stmt bindDouble(bindIndex, val)
                    "arg #{argIndex} number = #{val}" println()
                case duk isString(argIndex) =>
                    val := duk requireString(argIndex)
                    stmt bindText(bindIndex, val)
                    "arg #{argIndex} string = #{val}" println()
                case duk isNull(argIndex) =>
                    "arg #{argIndex} null" println()
                    stmt bindNull(bindIndex)
                case =>
                    raise("Unknown type in exec argument")
            }
            bindIndex += 1
        }
       
        // results array
        dukResult := duk pushObject()
        "dukResult = #{dukResult}" println()
        
        dukRows := duk pushArray()
        rowCount := 0

        first := true

        while (true) {
            res := stmt step()
            if (res == Sqlite3Code row) {
                "got row" println()
                numColumns := stmt columnCount()
                "numColumns = #{numColumns}" println()

                if (first) {
                    "was first, not anymore" println()
                    first = false
                    dukCols := duk pushArray()
                    "dukCols = #{dukCols}" println()
                    for (colIndex in 0..numColumns) {
                        dukCol := duk pushObject()
                        "dukCol = #{dukCol}" println()
                        
                        key := stmt columnName(colIndex)
                        duk pushString(key)
                        duk putPropString(dukCol, "name")
                        "pushed name = #{key}" println()

                        typ := stmt columnType(colIndex)
                        duk pushString(
                            match (typ) {
                                case Sqlite3Type _integer =>
                                    "INTEGER"
                                case Sqlite3Type _float =>
                                    "FLOAT"
                                case Sqlite3Type _blob =>
                                    "BLOB"
                                case Sqlite3Type _text =>
                                    "TEXT"
                                case =>
                                    raise("Type not supported")
                                    ""
                            }
                        )
                        "pushed type" println()
                        duk putPropString(dukCol, "type")
                        duk putPropIndex(dukCols, colIndex)
                    }
                    duk putPropString(dukResult, "columns")
                    "put columns" println()
                }

                dukRow := duk pushArray()
                "created dukRow = #{dukRow}" println()
                for (colIndex in 0..numColumns) {
                    "in column #{colIndex}" println()
                    val := stmt valueColumn(colIndex)
                    type := val type()
                    match (type) {
                        case Sqlite3Type _integer =>
                            duk pushNumber(val toInt() as Double)
                        case Sqlite3Type _float =>
                            duk pushNumber(val toDouble())
                        case Sqlite3Type _blob =>
                            raise("Blob type not supported!")
                        case Sqlite3Type _text =>
                            duk pushString(val toString())
                        case Sqlite3Type _null =>
                            duk pushNull()
                        case =>
                            raise("Type not supported")
                    }
                    duk putPropIndex(dukRow, colIndex)
                    "put col in dukrow" println()
                }
                duk putPropIndex(dukRows, rowCount)
                "put dukrow in dukrows" println()
                rowCount += 1
            } else {
                break
            }

            // TODO: handle misuse, etc.
        }
        stmt finalize()

        "put dukRows in dukResult" println()
        duk putPropString(dukResult, "rows")

        1
    }

}

