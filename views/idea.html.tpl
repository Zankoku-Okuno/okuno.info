% rebase('base.html.tpl')
<h2>Idea</h2>

<form method=POST action={{app.get_url('ed_idea', id=idea['id'])}}>
    <textarea required name=text
              rows=5 cols=60 style="resize: both;"
              placeholder="new action">{{idea['text']}}</textarea>
    </br>
    <button type=submit>Edit</button>
</form>
