% rebase('base.html.tpl')
<h2>Projects</h2>


<ul class='projects'>
    %for project in projects:
    <li class='project'>
        %include("project.frag.html", project=project)
    </li>
    %end
    
    <li class='project'>
        <div class="collapse-head collapse-selector"
             data-collapse=new-project>
            <input type=checkbox>New Project
        </div>
        <form method=POST action="{{app.get_url('mk_project')}}"
              class='collapse'
              data-collapse=new-project>
            <input type=text required name=name
                   placeholder="new project" /></br>
            <textarea name=description
                      rows=5 cols=60 style="resize: both;"
                      placeholder="description"></textarea></br>
            <button type=submit>Create</button>
        </form>
    </li>
</ul>
