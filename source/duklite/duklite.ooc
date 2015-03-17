
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
                case =>
                    raise("Unknown type in exec argument")
            }
            bindIndex += 1
        }
       
        // results array
        dukResults := duk pushArray()
        rowCount := 0

        while (true) {
            res := stmt step()
            if (res == Sqlite3Code row) {
                dukRow := duk pushObject()
                numColumns := stmt columnCount()
                for (colIndex in 0..numColumns) {
                    key := stmt columnName(colIndex)
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
                    duk putPropString(dukRow, key)
                }
                duk putPropIndex(dukResults, rowCount)
                rowCount += 1
            } else {
                break
            }

            // TODO: handle misuse, etc.
        }
        stmt finalize()

        // todo: fill with result
        1
    }

}

