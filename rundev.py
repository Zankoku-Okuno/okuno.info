from app import app

from bottle import run
run(app, host="localhost", port=8080)

