
use sqlite3
import sqlite3/sqlite3

use duktape
import duk/tape

DB: class {

    db: Database 

    init: func (path: String) {
        db = Database new(path)
    }

    exec: func (duk: DukContext) -> Int {
        query := duk requireString(0)
        stmt := db prepare(query toString())

        bindIndex := 1
        numArgs := duk getTop() - 1
        for (argIndex in 1..numArgs) {
            match {
                case duk isNumber(argIndex) =>
                    val := duk requireNumber(argIndex)
                    stmt bindDouble(bindIndex, val)

                case duk isString(argIndex) =>
                    val := duk requireString(argIndex)
                    stmt bindText(bindIndex, val)

                case duk isNull(argIndex) =>
                    stmt bindNull(bindIndex)

                case duk isBoolean(argIndex) =>
                    val := duk requireBoolean(argIndex)
                    stmt bindInt(bindIndex, val as Int)

                case duk isBuffer(argIndex) =>
                    bufSize: SizeT
                    val := duk requireBuffer(argIndex, bufSize&)
                    stmt bindBlob(bindIndex, val, bufSize as Int)
                    
                case duk isUndefined(argIndex) =>
                    duk throwError("arg #{argIndex} to #{query} is undefined")

                case =>
                    duk throwError("arg #{argIndex} to #{query} is of unknown type")
            }
            bindIndex += 1
        }
       
        // results array
        dukResult := duk pushObject()
        
        dukRows := duk pushArray()
        rowCount := 0

        first := true

        while (true) {
            res := stmt step()

            match res {
                case Sqlite3Code row =>
                    numColumns := stmt columnCount()

                    if (first) {
                        first = false
                        dukCols := duk pushArray()
                        for (colIndex in 0..numColumns) {
                            dukCol := duk pushObject()

                            key := stmt columnName(colIndex)
                            duk pushString(key)
                            duk putPropString(dukCol, "name")

                            type := stmt columnType(colIndex)
                            duk pushString(
                                match (type) {
                                    case Sqlite3Type _integer =>
                                        "INTEGER"
                                    case Sqlite3Type _float =>
                                        "FLOAT"
                                    case Sqlite3Type _blob =>
                                        "BLOB"
                                    case Sqlite3Type _text =>
                                        "TEXT"
                                    case Sqlite3Type _null =>
                                        "NULL"
                                    case =>
                                        duk throwError("When doing columns, sqlite type #{type} not supported")
                                        ""
                                }
                            )
                            duk putPropString(dukCol, "type")
                            duk putPropIndex(dukCols, colIndex)
                        }
                        duk putPropString(dukResult, "columns")
                    }

                    dukRow := duk pushArray()
                    for (colIndex in 0..numColumns) {
                        // TODO: sqlite docs say not to use value column.
                        val := stmt valueColumn(colIndex)
                        type := val type()
                        match (type) {
                            case Sqlite3Type _integer =>
                                duk pushNumber(val toInt() as Double)
                            case Sqlite3Type _float =>
                                duk pushNumber(val toDouble())
                            case Sqlite3Type _blob =>
                                bufSize := stmt columnBytes(colIndex)
                                src := val toBlob()
                                dst := duk pushFixedBuffer(bufSize)
                                memcpy(dst, src, bufSize)
                            case Sqlite3Type _text =>
                                duk pushString(val toString())
                            case Sqlite3Type _null =>
                                duk pushNull()
                            case =>
                                duk throwError("When doing row, sqlite type #{type} not supported")
                        }
                        duk putPropIndex(dukRow, colIndex)
                    }
                    duk putPropIndex(dukRows, rowCount)
                    rowCount += 1
                case Sqlite3Code done =>
                    // all good!
                    break
                case =>
                    duk throwError("sqlite error: #{db errmsg()}")
            }

            // TODO: handle misuse, etc.
        }
        stmt finalize()

        duk putPropString(dukResult, "rows")

        1
    }

    lastInsertRowId: func -> Int {
        db lastInsertRowId()
    }

}

