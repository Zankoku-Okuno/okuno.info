import bottle
from bottle import request, response, view
from .framework import app, sql
from .auth import login, login_required

from urllib.parse import quote_plus as percent_encode, unquote_plus as percent_decode
import json
import sqlite3
from datetime import datetime, timedelta


timeformat = "%Y-%m-%dT%H:%M:%S+00:00"
dbfile = "db.sqlite"


# ====== Administrivia ======

@app.route('/', name='index')
@view("index.html")
def index():
    return dict()

@app.get('/login', name='login')
def login_form():
    return """
        <form action='' method=POST>
            <input type=text name=uname />
            <input type=password name=pword />
            <input type=submit value=Login />
        </form>"""

@app.post('/login')
def login_attempt():
    if login():
        next = request.params.get('next')
        if next is not None:
            next = percent_decode(next)
        else:
            next = app.get_url('index')
        bottle.redirect(next)
    else:
        bottle.redirect(app.get_url('login'))


# ====== Ideas ======

@app.get('/ideas', name='ideas')
@login_required
@view("ideas.html")
def ideas():
    with sql(dbfile) as db:
        dayAgo = (datetime.utcnow() - timedelta(hours=16)).strftime(timeformat)
        ideas = db.execute("""SELECT * FROM idea
                              WHERE (project_id IS NULL AND crankfile = 0)
                                 OR sorted > ?
                              ORDER BY created ASC;""", (dayAgo,)).fetchall()
        projects = db.execute("""SELECT * FROM project
                                 ORDER BY name ASC;""").fetchall()
    return dict(ideas=ideas, projects=projects)

@app.post('/ideas', name='mk_idea')
@login_required
def mk_idea():
    now = datetime.utcnow().strftime(timeformat)
    text = request.params.get('text')
    if not text:
        bottle.abort(400, "Missing `text` field")

    with sql(dbfile) as db:
        id = db.execute("""INSERT INTO idea (text, created) VALUES (?, ?)""", (text, now)).lastrowid
    
    next = request.headers.get('Referer', app.get_url('idea', id=id))
    bottle.redirect(next)

@app.get('/idea/<id:int>', name='idea')
@login_required
@view("idea.html")
def idea(id):
    with sql(dbfile) as db:
        idea = db.execute("""SELECT * FROM idea WHERE id = ?;""", (id,)).fetchone()
        if idea is None:
            bottle.abort(404)
    return dict(idea=idea)

@app.post('/idea/<id:int>', name='ed_idea')
@login_required
def ed_idea(id):
    with sql(dbfile) as db:
        idea = db.execute("""SELECT id FROM idea WHERE id = ?;""", (id,)).fetchone()
        if idea is None:
            bottle.abort(400, "Missing `idea` field")
        text = request.params.get('text', "")
        project = request.params.get('project')
        now = datetime.utcnow().strftime(timeformat)

        if project is not None:
            if project == 'crankfile':
                db.execute("""UPDATE idea SET project_id = NULL, crankfile = 1, sorted = ?
                              WHERE id = ?;""", (now, idea['id']))
            else:
                project = db.execute("""SELECT id FROM project WHERE id = ?;""", (project,)).fetchone()
                if project is None:
                    bottle.abort(404)
                db.execute("""UPDATE idea SET project_id = ?, crankfile = 0, sorted = ?
                              WHERE id = ?;""", (project['id'], now, idea['id']))
        if text:
            db.execute("""UPDATE idea SET text = ? WHERE id = ?;""", (text, id,))

    next = request.headers.get('Referer', app.get_url('idea', id=id))
    bottle.redirect(next)


# ====== Projects ======

@app.get('/projects', name='projects')
@login_required
@view("projects.html")
def projects():
    with sql(dbfile) as db:
        rows = db.execute("""SELECT project.*, count(action.id) as actions_count
                             FROM project LEFT JOIN action
                                          ON (project.id = action.project_id
                                              AND action.completed IS NULL)
                             GROUP BY project.id
                             ORDER BY project.name ASC;""").fetchall()
    return dict(projects=rows)

@app.get('/projects/<id:int>', name='project')
@login_required
@view("project.html")
def project(id):
    with sql(dbfile) as db:
        project = db.execute("""SELECT * FROM project WHERE id = ?;""", (id,)).fetchone()
        if project is None:
            bottle.abort(404, "No such project")
        actions = db.execute("""SELECT * FROM action
                                WHERE project_id = ?
                                ORDER BY completed ASC;""", (id,)).fetchall()
        ideas = db.execute("""SELECT * FROM idea WHERE project_id = ?;""", (id,)).fetchall()
    return dict(project=project, actions=actions, ideas=ideas)

@app.post("/projects", name='mk_project')
@login_required
def mk_project():
    name = request.params.get('name')
    if not name:
        bottle.abort(400, "Missing `name` field")
    description = request.params.get('description', "")

    with sql(dbfile) as db:
        id = db.execute("""INSERT INTO project (name, description) VALUES (?, ?);""", (name, description)).lastrowid

    next = request.headers.get('Referer', app.get_url('project', id=id))
    bottle.redirect(next)

@app.post("/project/<id:int>", name='ed_project')
@login_required
def ed_project(id):
    with sql(dbfile) as db:
        project = db.execute("""SELECT * FROM project WHERE id = ?;""", (id,)).fetchone()
        if project is None:
            bottle.abort(404)
        name = request.params.get('name')
        description = request.params.get('description')

        if name:
            db.execute("""UPDATE project SET name = ? WHERE id = ?;""", (name, id,))
        if description:
            db.execute("""UPDATE project SET description = ? WHERE id = ?;""", (description, id,))

    next = request.headers.get('Referer', app.get_url('action', id=id))
    bottle.redirect(next)


# ====== Actions ======

@app.get("/action/<id:int>", name="action")
@login_required
@view("action.html")
def action(id):
    with sql(dbfile) as db:
        action = db.execute("""SELECT action.*, project.name as project_name
                               FROM action JOIN project ON project.id = action.project_id
                               WHERE action.id = ?;""", (id,)).fetchone()
        if action is None:
            bottle.abort(404)
    return dict(action=action)

@app.post("/actions", name='mk_action')
@login_required
def mk_action():
    project_id = request.params.get('project_id')
    if project_id is None:
        bottle.abort(400, "Missing `project_id` field")
    text = request.params.get('text')
    if not text:
        bottle.abort(400, "Missing `text` field")
    now = datetime.utcnow().strftime(timeformat)

    with sql(dbfile) as db:
        id = db.execute("""INSERT INTO action (project_id, text, created)
                           VALUES (?, ?, ?);""", (project_id, text, now)).lastrowid

    next = request.headers.get('Referer', app.get_url('action', id=id))
    bottle.redirect(next)

@app.post("/action/<id:int>", name='ed_action')
@login_required
def ed_action(id):
    with sql(dbfile) as db:
        action = db.execute("""SELECT * FROM action WHERE id = ?;""", (id,)).fetchone()
        if action is None:
            bottle.abort(404)

        completed = request.params.get('completed')
        if completed is not None:
            if completed.lower() in {'1', 'yes', 'true'}:
                completed = datetime.utcnow().strftime(timeformat)
            else:
                completed = None
            db.execute("""UPDATE action SET completed = ? WHERE id = ?;""", (completed, id))
        text = request.params.get('text')
        if text:
            db.execute("""UPDATE action SET text = ? WHERE id = ?;""", (text, id))

    next = request.headers.get('Referer', app.get_url('action', id=id))
    bottle.redirect(next)


# ====== Epigrams ======

@app.get("/epigrams", name='epigrams')
@login_required
@view("epigrams.html")
def epigrams():
    with sql(dbfile) as db:
        epigrams = db.execute("""SELECT * FROM epigram;""").fetchall()
    return dict(epigrams=epigrams)

@app.post("/epigrams", name='mk_epigram')
@login_required
def mk_epigram():
    text = request.params.get('text', "")
    if not text:
        bottle.abort(400, "Missing `text` field")
    credit = request.params.get('credit', "")
    if not credit:
        credit = None

    with sql(dbfile) as db:
        id = db.execute("""INSERT INTO epigram (text, credit) VALUES (?, ?);""", (text, credit)).lastrowid

    next = request.headers.get('Referer', app.get_url('epigrams'))
    bottle.redirect(next)

