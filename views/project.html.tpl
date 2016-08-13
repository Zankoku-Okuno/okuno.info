%rebase('base.html.tpl')
<h2>Project</h2>

<div class='project_desc'>
    %include('project.frag.html', project=project)
</div>

%if ideas:
<h3>Ideas</h3>
<ul class='ideas'>
    %for idea in ideas:
    <li class='idea'>
        %include("idea.frag.html", idea=idea)
    </li>
    %end
    <li class='idea'>
        <div class='collapse-head collapse-selector'
             data-collapse=new-idea>
            <input type=checkbox>New Idea
        </div>
        <form method=POST action="{{app.get_url('mk_idea')}}"
              class='collapse'
              data-collapse=new-idea>
            <input type=hidden name=project_id value={{project['id']}} />
            <textarea name=text required
                      rows=5 cols=60 style="resize: both;"
                      placeholder="new idea"></textarea><br/>
            <button type=submit>Create</button>
        </form>
    </li>
</ul>
%end

<h3>Actions</h3>
<form method=POST action="{{app.get_url('mk_action')}}">
    <input type=hidden name=project_id value={{project['id']}} />
    <textarea required name=text
              rows=5 cols=60 style="resize: both;"
              placeholder="new action"></textarea></br>
    <button type=submit>Create</button>
</form>

<ol>
    %for action in actions:
    <li>
        <!--<pre>{{action['text']}}</pre>-->
        %if action['completed'] is None:
        <form method=POST action="{{app.get_url('ed_action', id=action['id'])}}">
            <button type=submit name='completed' value=1>☐</button> {{action['text']}} <small><a href={{app.get_url('action', id=action['id'])}}>edit</a></small>
        </form>
        %else:
        <button>☑</button> {{action['text']}} <small><a href={{app.get_url('action', id=action['id'])}}>edit</a></small>
        %end
    </li>
    %end
</ol>


