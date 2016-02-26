<script type="text/javascript">
function localPath(prefix) {
	document.getElementById('form.archived').value = prefix + document.getElementById('form.date').value + '/';
}
</script>

Type: <dfPlainText ref="type" />

<dfLabel ref="date">Date</dfLabel>
<td><dfInput type="date" ref="date" required />

<dfLabel ref="date">Description</dfLabel>
<dfInputTextArea ref="description" required class="maximum" />

<fieldset>
	<legend>Tags</legend>

	<dfInputList ref="tags"><ul class="tags">
		<dfListItem><li itemAttrs>
			<label> <dfInputCheckbox ref="item" /> <dfPlainText ref="name" /></label>
		</li></dfListItem>
	</ul></dfInputList>
</fieldset>

<fieldset>
	<legend>Visibility</legend>

	<dfLabel ref="archived">Archived</dfLabel>
	<dfInputText ref="archived" /> <input type="button" value="Local" onclick="localPath('/${archivePath}${slug}/')" />

	<ul>
		<li><label><dfInputCheckbox ref="public" /> Public</label></li>
		<li><label><dfInputCheckbox ref="public" disabled /> Featured</label></li>
	</ul>
</fieldset>
