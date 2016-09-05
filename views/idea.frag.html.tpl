<div class="tabs-head"
     data-tabs=idea{{idea.id}}>
    <input type=radio name=tab-idea{{idea.id}}
           data-tabs=idea{{idea.id}}
           value=view checked />
    <input type=radio name=tab-idea{{idea.id}}
           data-tabs=idea{{idea.id}}
           value=edit />
</div>

<div class="tabpane"
     data-tabs=idea{{idea.id}}
     data-tabpane=view>
    <div class="view-item">
      <div class="md">{{idea.text}}</div>
      <small class='tab-selector'
         data-tabs=idea{{idea.id}}
         data-tabpane=edit>edit
      </small>
    </div>
    <p>
        %if idea.sorted is None:
        <form method=POST action={{app.get_url('ed_idea', id=idea.id)}}>
          %include("sort-select.frag.html", idea=idea, sort_to=sort_to)
          <button type=submit>Sort</button>
        </form>
        %elif idea.crankfile:
        sorted to: Crankfile
        %else:
        sorted to: {{idea.project_name}}
        %end
    </p>
    <!--TODO date of creation-->
</div>

<div class="tabpane"
     data-tabs=idea{{idea.id}}
     data-tabpane=edit>
    <form method=POST action={{app.get_url('ed_idea', id=idea.id)}}>
      <button type=submit>Edit</button>
      <small class='tab-selector'
         data-tabs=idea{{idea.id}}
         data-tabpane=view>back
      </small><br/>
      <textarea required name=text
                rows=5 cols=60 style="resize: both;"
                placeholder="new action">{{idea.text}}</textarea>
      <br/>
      Sort to:
      %include("sort-select.frag.html", idea=idea, sort_to=sort_to)
    </form>

    <div style="border: thin red solid;">
        <h6 class='collapse-head collapse-selector'
            data-collapse=del-idea{{idea.id}}>
            <input type=checkbox>Danger Zone
        </h6>
        <form method=POST action={{app.get_url('ed_idea', id=idea.id)}}
              class='collapse'
              data-collapse=del-idea{{idea.id}}>
            <select name=delete>
                <option value='no' selected></option>
                <option value='yes'>Delete</option>
            </select>
            <button type=submit>Confirm</button>
        </form>
    </div>
</div>
