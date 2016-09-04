import bottle
from bottle import Bottle

app = Bottle(catchall=False)
bottle.BaseTemplate.defaults['app'] = app
bottle.BaseTemplate.defaults['request'] = bottle.request



from contextlib import contextmanager
import bottle, sqlite3

class Model:
    def __init__(self, **kwargs):
        for k, v in kwargs.items():
            setattr(self, k, v)

    def __getitem__(self, k):
        return getattr(self, k)

    @staticmethod
    def from_sql(cursor, row):
        d = {}
        for idx, col in enumerate(cursor.description):
            d[col[0]] = row[idx]
        return Model(**d)

@contextmanager
def sql(dbfilename):
    db = sqlite3.connect(dbfilename)
    db.row_factory = Model.from_sql
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
