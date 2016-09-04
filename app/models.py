from datetime import datetime, timedelta

# FIXME put this in the framework or something
timeformat = "%Y-%m-%dT%H:%M:%S+00:00"



class Idea:
    def __init__(self, db):
        self._db = db

    def by_id(self, id):
        return self._db.execute(
            """SELECT idea.*, project.name as project_name
               FROM idea LEFT JOIN project ON (project.id = idea.project_id)
               WHERE idea.id = ?;""",
            (id,)
        ).fetchone()

    def create(self, project, text):
        now = datetime.utcnow().strftime(timeformat)
        if project is None:
            id = self._db.execute("""INSERT INTO idea (text, created)
                               VALUES (?, ?)""", (text, now)).lastrowid
        elif project == 'crankfile':
            id = self._db.execute("""INSERT INTO idea (text, crankfile, created, sorted)
                               VALUES (?, ?, ?, ?)""", (text, 1, now, now)).lastrowid
        else:
            id = self._db.execute("""INSERT INTO idea (text, project, created, sorted)
                               VALUES (?, ?, ?, ?)""", (text, project, now, now)).lastrowid
        return id

    def update_by_id(self, id, project=None, text=None):
        now = datetime.utcnow().strftime(timeformat)
        if project == 'crankfile':
            self._db.execute(
                """UPDATE idea SET project_id = NULL, crankfile = 1, sorted = ?
                   WHERE id = ?;""",
                (now, id)
            )
        elif project is not None:
            self._db.execute(
                """UPDATE idea SET project_id = ?, crankfile = 0, sorted = ?
                   WHERE id = ?;""",
                (project, now, id)
            )
        
        if text:
            self._db.execute("""UPDATE idea SET text = ? WHERE id = ?;""", (text, id,))

    def delete_by_id(self, id):
        self._db.execute("""DELETE FROM idea WHERE id = ?;""", (id,))


    def unsorted_all(self):
        minAgo = (datetime.utcnow() - timedelta(minutes=1)).strftime(timeformat)
        return self._db.execute(
            """SELECT idea.*, project.name as project_name
               FROM idea LEFT JOIN project
                         ON (project.id = idea.project_id)
               WHERE (project_id IS NULL AND crankfile = 0)
                  OR sorted > ?
               ORDER BY created ASC;""",
            (minAgo,)
        ).fetchall()

    def crankfile_all(self):
        return self._db.execute(
            """SELECT idea.*, project.name as project_name
               FROM idea LEFT JOIN project ON (project.id = idea.project_id)
               WHERE crankfile != 0
               ORDER BY created DESC;"""
        ).fetchall()

class Project:
    def __init__(self, db):
        self._db = db

    def by_id(self, id):
        return self._db.execute(
            """SELECT * FROM project WHERE id = ?;""",
            (id,)
        ).fetchone()

    def all_names(self):
        return self._db.execute(
            """SELECT id, name FROM project
               ORDER BY name ASC;"""
        ).fetchall()
