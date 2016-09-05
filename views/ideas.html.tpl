% rebase('base.html.tpl')
<h2>Ideas</h2>

<p><a href={{app.get_url('crankfile')}}>Crankfile</a></p>

<ul class='ideas'>
    <li class='idea'>
        <div class='collapse-head collapse-selector'
             data-collapse=new-idea>
            <input type=checkbox checked>New Idea
        </div>
        <form method=POST action="{{app.get_url('mk_idea')}}"
              class='collapse'
              data-collapse=new-idea>
            <textarea name=text required
                      rows=5 cols=60 style="resize: both;"
                      placeholder="new idea"></textarea><br/>
            <button type=submit>Create</button>
        </form>
    </li>
    %for idea in ideas:
    <li class='idea'>
        %include("idea.frag.html", idea=idea)
    </li>
    %end
</ul>
