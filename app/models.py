from .framework import Model
from datetime import datetime, timedelta

# FIXME put this in the framework or something
timeformat = "%Y-%m-%dT%H:%M:%S+00:00"



class Idea(Model):
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

    def by_project_all(self, id):
        return self._db.execute(
            """SELECT idea.*, project.name as project_name
               FROM idea LEFT JOIN project ON (project.id = idea.project_id)
               WHERE project_id = ?;""",
            (id,)
        ).fetchall()

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





class Project(Model):
    def by_id(self, id):
        return self._db.execute(
            """SELECT * FROM project WHERE id = ?;""",
            (id,)
        ).fetchone()

    def all(self):
        return self._db.execute(
            """SELECT project.*, count(action.id) as actions_count
               FROM project LEFT JOIN action
                            ON (project.id = action.project_id
                                AND action.completed IS NULL)
               GROUP BY project.id
               ORDER BY project.name ASC;""").fetchall()

    def create(self, name, description):
        return self._db.execute(
            """INSERT INTO project (name, description) VALUES (?, ?);""",
            (name, description)
        ).lastrowid

    def update_by_id(self, id, name=None, description=None):
        if name:
            self._db.execute("""UPDATE project SET name = ? WHERE id = ?;""", (name, id,))
        if description is not None:
            self._db.execute("""UPDATE project SET description = ? WHERE id = ?;""", (description, id,))


    def by_id_full(self, id):
        project = Project(self._db).by_id(id)
        if project is None:
            return None
        project.actions = Action(self._db).by_project_all(project.id)
        project.ideas = Idea(self._db).by_project_all(project.id)
        return project

    def all_names(self):
        return self._db.execute(
            """SELECT id, name FROM project
               ORDER BY name ASC;"""
        ).fetchall()





class Action(Model):
    def by_id(self, id):
        return self._db.execute(
            """SELECT action.*, project.name as project_name
               FROM action JOIN project ON project.id = action.project_id
               WHERE action.id = ?;""",
            (id,)
        ).fetchone()

    def create(self, project, text):
        now = datetime.utcnow().strftime(timeformat)
        return self._db.execute(
            """INSERT INTO action (project_id, text, created)
               VALUES (?, ?, ?);""",
            (project, text, now)
        ).lastrowid

    def update_by_id(self, id, completed=None, text=None):
        if completed is not None:
            completed = datetime.utcnow().strftime(timeformat) if completed else None
            self._db.execute(
                """UPDATE action SET completed = ? WHERE id = ?;""",
                (completed, id)
            )
        if text:
            self._db.execute(
                """UPDATE action SET text = ? WHERE id = ?;""",
                (text, id)
            )

    def delete_by_id(self, id):
        self._db.execute(
            """DELETE FROM action WHERE id = ?;""",
            (id,)
        )

    def by_project_all(self, id):
        return self._db.execute(
            """SELECT * FROM action
               WHERE project_id = ?
               ORDER BY completed ASC;""",
            (id,)
        ).fetchall()





class Epigram(Model):
    def all(self):
        return self._db.execute("""SELECT * FROM epigram;""").fetchall()

    def by_id(self, id):
        return self._db.execute(
            """SELECT * FROM epigram WHERE id = ?;""",
            (id,)
        ).fetchone()

    def create(self, text, credit):
        return self._db.execute(
            """INSERT INTO epigram (text, credit) VALUES (?, ?);""",
            (text, credit)
        ).lastrowid

    def update_by_id(self, id, text=None, credit=None):
        if text:
            self._db.execute("""UPDATE epigram SET text = ? WHERE id = ?;""", (text, id))
        if credit is not None:
            self._db.execute("""UPDATE epigram SET credit = ? WHERE id = ?;""", (credit, id))