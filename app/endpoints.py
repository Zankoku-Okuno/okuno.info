import bottle
from bottle import request, response, view
from .framework import app, sql
from .auth import login, logout, login_required
from .models import *

from urllib.parse import quote_plus as percent_encode, unquote_plus as percent_decode
import json
import sqlite3
from datetime import datetime, timedelta


timeformat = "%Y-%m-%dT%H:%M:%S+00:00"

def dbfile():
    return "db/{}.db".format(request.username)



# ====== Administrivia ======

@app.route('/static/<path:path>', name='static')
def static(path):
    """static file fallback"""
    return bottle.static_file(path, root='static')

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

@app.post('/logout', name='logout')
def logout_button():
    logout()
    bottle.redirect(app.get_url('index'))


# ====== Ideas ======

@app.get('/ideas', name='ideas')
@login_required
@view("ideas.html")
def ideas():
    with sql(dbfile()) as db:
        ideas = Idea(db).unsorted_all()
        projects = Project(db).all_names()
    return dict(ideas=ideas, sort_to=projects)

@app.get('/ideas/crankfile', name='crankfile')
@login_required
@view("crankfile.html")
def crankfile():
    with sql(dbfile()) as db:
        ideas = Idea(db).crankfile_all()
        projects = Project(db).all_names()
    return dict(ideas=ideas, sort_to=projects)

@app.post('/ideas', name='mk_idea')
@login_required
def mk_idea():
    with sql(dbfile()) as db:
        project_id = request.params.get('project_id')
        text = request.params.get('text') or bottle.abort(400, "Missing `text` field")
        #FIXME check that the project exists

        id = Idea(db).create(project_id, text)
        #FIXME check that creation succeeded

    next = request.headers.get('Referer', app.get_url('idea', id=id))
    bottle.redirect(next)

@app.get('/idea/<id:int>', name='idea')
@login_required
@view("idea.html")
def idea(id):
    with sql(dbfile()) as db:
        idea = Idea(db).by_id(id)
        if idea is None:
            bottle.abort(404)
        projects = Project(db).all_names()
    return dict(idea=idea, sort_to=projects)

@app.post('/idea/<id:int>', name='ed_idea')
@login_required
def ed_idea(id):
    with sql(dbfile()) as db:
        idea = Idea(db).by_id(id) or bottle.abort(404)
        text = request.params.get('text', "")
        project = request.params.get('project') #FIXME check that the project exists
        delete = request.params.get('delete', 'no')

        if delete.lower() in {'1', 'yes', 'true'}:
            Idea(db).delete_by_id(idea.id)
            bottle.redirect(app.get_url('index')) #FIXME more precise redirect
        else:
            Idea(db).update_by_id(idea.id, project=project, text=text)
            next = request.headers.get('Referer', app.get_url('idea', id=idea.id))
            bottle.redirect(next)
        #FIXME check that edit/delete succeeded
            

# ====== Projects ======

@app.get('/projects', name='projects')
@login_required
@view("projects.html")
def projects():
    with sql(dbfile()) as db:
        rows = Project(db).all()
    return dict(projects=rows)

@app.get('/projects/<id:int>', name='project')
@login_required
@view("project.html")
def project(id):
    with sql(dbfile()) as db:
        project = Project(db).by_id_full(id) or bottle.abort(404, "No such project")
        sort_to = Project(db).all_names()
    #FIXME send actions & ideas along with the project object
    return dict(project=project, actions=project.actions, ideas=project.ideas, sort_to=sort_to)

@app.post("/projects", name='mk_project')
@login_required
def mk_project():
    name = request.params.get('name') or bottle.abort(400, "Missing `name` field")
    description = request.params.get('description', "")

    with sql(dbfile()) as db:
        id = Project(db).create(name=name, description=description)

    next = request.headers.get('Referer', app.get_url('project', id=id))
    bottle.redirect(next)

@app.post("/project/<id:int>", name='ed_project')
@login_required
def ed_project(id):
    with sql(dbfile()) as db:
        project = Project(db).by_id(id) or bottle.abort(404)
        name = request.params.get('name')
        description = request.params.get('description')
        Project(db).update_by_id(id, name, description)

    next = request.headers.get('Referer', app.get_url('action', id=id))
    bottle.redirect(next)


# ====== Actions ======

@app.get("/action/<id:int>", name="action")
@login_required
@view("action.html")
def action(id):
    with sql(dbfile()) as db:
        action = Action(db).by_id(id) or bottle.abort(404)
    return dict(action=action)

@app.post("/actions", name='mk_action')
@login_required
def mk_action():
    project_id = request.params.get('project_id') or bottle.abort(400, "Missing `project_id` field")
    text = request.params.get('text') or bottle.abort(400, "Missing `text` field")

    with sql(dbfile()) as db:
        id = Action(db).create(project=project_id, text=text)

    next = request.headers.get('Referer', app.get_url('action', id=id))
    bottle.redirect(next)

@app.post("/action/<id:int>", name='ed_action')
@login_required
def ed_action(id):
    with sql(dbfile()) as db:
        action = Action(db).by_id(id) or bottle.abort(404)
        completed = request.params.get('completed')
        if completed is not None:
            completed = completed.lower() in {'1', 'yes', 'true'}
        text = request.params.get('text')
        delete = request.params.get('delete', 'no')

        if delete.lower() in {'1', 'yes', 'true'}:
            Action(db).delete_by_id(id)
            bottle.redirect(app.get_url('project', id=action.project_id))
        else:
            Action(db).update_by_id(id, completed=completed, text=text)


    next = request.headers.get('Referer', app.get_url('action', id=id))
    bottle.redirect(next)


# ====== Epigrams ======

@app.get("/epigrams", name='epigrams')
@login_required
@view("epigrams.html")
def epigrams():
    with sql(dbfile()) as db:
        epigrams = Epigram(db).all()
    return dict(epigrams=epigrams)

@app.post("/epigrams", name='mk_epigram')
@login_required
def mk_epigram():
    text = request.params.get('text', "") or bottle.abort(400, "Missing `text` field")
    credit = request.params.get('credit') or None

    with sql(dbfile()) as db:
        id = Epigram(db).create(text, credit)

    next = request.headers.get('Referer', app.get_url('epigrams'))
    bottle.redirect(next)

@app.get("/epigram/<id:int>", name='epigram')
@login_required
@view("epigram.html")
def epigram(id):
    with sql(dbfile()) as db:
        epigram = Epigram(db).by_id(id) or bottle.abort(404)
    return dict(epigram=epigram)

@app.post("/epigram/<id:int>", name='ed_epigram')
@login_required
def ed_epigram(id):
    with sql(dbfile()) as db:
        epigram = Epigram(db).by_id(id) or bottle.abort(404)
        text = request.params.get('text')
        credit = request.params.get('credit')
        Epigram(db).update_by_id(id, text=text, credit=credit)
        
    next = request.headers.get('Referer', app.get_url('epigram', id=id))
    bottle.redirect(next)

