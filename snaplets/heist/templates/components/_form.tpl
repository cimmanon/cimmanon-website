Type: <dfPlainText ref="component" />

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

	<ul>
		<!--<li><label><dfInputCheckbox ref="public" /> Public</label></li>
		<li><label><dfInputCheckbox ref="featured" /> Featured</label></li>-->
		<li><label><dfInputCheckbox ref="archived" /> Archived</label></li>
	</ul>
</fieldset>
