def login():
    return True

def logout():
    pass

def login_required(endpoint):
    from bottle import request
    def g(*args, **kwargs):
        request.username = 'okuno'
        return endpoint(*args, **kwargs)
    return g