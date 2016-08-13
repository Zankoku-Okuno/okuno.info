% rebase('base.html.tpl')
<h2>Action</h2>
<h3>{{action['project_name']}}</h3>
%include("action.frag.html", action=action)