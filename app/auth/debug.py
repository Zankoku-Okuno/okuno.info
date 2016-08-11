import os


def login():
    return True

def logout():
    pass

def login_required(endpoint):
    from bottle import request
    def g(*args, **kwargs):
        request.username = os.environ['DEBUG']
        return endpoint(*args, **kwargs)
    return g