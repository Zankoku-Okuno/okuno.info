import os
if os.environ.get('DEBUG'):
    from . import debug as auth
else:
    from . import real as auth

login = auth.login
logout = auth.logout
login_required = auth.login_required