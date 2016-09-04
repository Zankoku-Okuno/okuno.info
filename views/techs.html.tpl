% rebase('base.html.tpl')
<h2>Technologies</h2>


<ul class='techs'>
    %for tech in techs:
    <li class='tech'>
        <h3>
            <a href={{app.get_url('tech', id=tech['id'])}}>{{tech['name']}}</a>
        </h3>
    </li>
    %end

    <li>
        <h3 class="collapse-head collapse-selector"
            data-collapse=new-tech>
            <input type=checkbox data-collapse=new-tech>New Technology
        </h3>
        <form method=POST action={{app.get_url('mk_tech')}}
              class="collapse"
              data-collapse=new-tech>
            <input type=text name='name' required>
            <button type=submit>Create</button>
        </form>
    </li>
</ul>
