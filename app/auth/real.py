import bcrypt
from cryptography.fernet import Fernet, InvalidToken
import json


def crypto_eq(a, b):
    if len(a) != len(b):
        return False
    result = 1
    for x, y in zip(a, b):
        result &= x == y
    return True if result else False


with open('secrets/key', 'rb') as f:
    key = f.read()
crypto = Fernet(key)


def load_pwordhash(uname):
    creds = { 'okuno': 'secrets/pw/okuno' }
    pwfile = creds.get(uname)
    if creds is None:
        return None
    with open(pwfile, 'rb') as f:
        pwordhash = f.read()
    pwordhash = crypto.decrypt(pwordhash)
    return pwordhash


def mk_pwordhash(pword):
    salt = bcrypt.gensalt(15)
    pword = pword.encode('UTF-8', errors='strict')
    pword = bcrypt.hashpw(pword, salt)
    pword = crypto.encrypt(pword)
    return pword

def check_credentials(uname, pword):
    if uname is None or pword is None:
        return None
    pword = pword.encode('UTF-8', errors='strict')
    testhash = load_pwordhash(uname)
    token = dict(uname=uname, pwordhash=testhash.decode('ASCII', errors='strict'))
    token = json.dumps(token)
    token = token.encode('UTF-8', errors='strict')
    token = crypto.encrypt(token)
    token = token.decode('UTF-8', errors='strict')
    goodpw = crypto_eq(bcrypt.hashpw(pword, testhash), testhash)
    return token if goodpw else None

def check_token(token):
    if token is None:
        return None, None
    token = token.encode('UTF-8', errors='strict')
    try: token = crypto.decrypt(token, ttl=16*60*60)
    except InvalidToken: return None, False
    token = token.decode('UTF-8', errors='strict')
    token = json.loads(token)
    uname = token.get('uname')
    pwordhash = token.get('pwordhash')
    if uname is None or pwordhash is None:
        return None, False
    pwordhash = pwordhash.encode('ASCII', errors='strict')
    testhash = load_pwordhash(uname)
    return uname, crypto_eq(pwordhash, testhash)


import bottle
from bottle import request
from urllib.parse import quote as percent_encode, unquote as percent_decode

def login():
    # FIXME possibly take uname and pword from the uri
    # https uris are encrypted, but what gets sent over DNS or stored in the browser?
    uname = request.params.get('uname')
    pword = request.params.get('pword')
    token = check_credentials(uname, pword)
    if token is not None:
        bottle.response.set_cookie('authtoken', token, path='/', httponly=True)
        return True
    else:
        return False

def logout():
    bottle.response.delete_cookie('authtoken', path='/')

def login_required(endpoint):
    def g(*args, **kwargs):
        authtoken = request.cookies.get('authtoken')
        uname, token = check_token(authtoken)
        if token:
            request.username = uname
            return endpoint(*args, **kwargs)
        else:
            bottle.redirect('/login?next={next}'.format(next=percent_encode(request.path)))
    return g



