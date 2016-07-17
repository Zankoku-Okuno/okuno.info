from bottle import Bottle

app = Bottle(catchall=False)




from contextlib import contextmanager
import bottle, sqlite3

@contextmanager
def sql(dbfilename):
    db = sqlite3.connect(dbfilename)
    db.row_factory = sqlite3.Row
    try:
        yield db
        db.commit()
    except sqlite3.IntegrityError as e:
        db.rollback()
        raise bottle.HTTPError(500, "Database Error", e)
    except bottle.HTTPError as e:
        raise
    except bottle.HTTPResponse as e:
        db.commit()
        raise
    finally:
        db.close()
