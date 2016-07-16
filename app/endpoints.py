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



@app.route('/')
@view("index.html")
def index():
    return dict()

@app.get('/login')
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
        bottle.redirect(percent_decode(request.params.get('next', '/')))
    else:
        return login_form()


@app.get('/ideas')
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
@app.post('/ideas')
@login_required
def mk_idea():
    now = datetime.utcnow().strftime(timeformat)
    text = request.forms.get('text')
    
    if not text:
        bottle.abort(400, "Missing `text` field")

    with sql(dbfile) as db:
        id = db.execute("""INSERT INTO idea (text, created) VALUES (?, ?)""", (text, now)).lastrowid
    bottle.redirect("/ideas")

@app.get('/projects')
@login_required
@view("projects.html")
def projects():
    with sql(dbfile) as db:
        rows = db.execute("""SELECT * FROM project
                             ORDER BY name ASC;""").fetchall()
    return dict(projects=rows)
@app.post("/projects")
@login_required
def mk_project():
    name = request.forms.get('name')
    description = request.forms.get('description', "")

    if not name:
        bottle.abort(400, "Missing `name` field")

    with sql(dbfile) as db:
        id = db.execute("""INSERT INTO project (name, description) VALUES (?, ?)""", (name, description)).lastrowid
    bottle.redirect("/projects")


@app.post("/ideas/sort")
@login_required
def sort_idea():
    idea = request.params.get('idea')
    project = request.params.get('project')

    if idea is None:
        bottle.abort(400, "Missing `idea` field")
    if project is None:
        bottle.abort(400, "Missing `project` field")

    with sql(dbfile) as db:
        now = datetime.utcnow().strftime(timeformat)
        print(repr(idea))
        idea = db.execute("""SELECT id FROM idea WHERE id = ?""", (idea,)).fetchone()
        if idea is None:
            bottle.abort(404)

        if project == 'crankfile':
            db.execute("""UPDATE idea SET project_id = NULL, crankfile = 1, sorted = ?
                          WHERE id = ?;""", (now, idea['id']))
        else:
            project = db.execute("""SELECT id FROM project WHERE id = ?""", (project,)).fetchone()
            if project is None:
                bottle.abort(404)
            db.execute("""UPDATE idea SET project_id = ?, crankfile = 0, sorted = ?
                          WHERE id = ?;""", (project['id'], now, idea['id']))
    bottle.redirect('/ideas')

