<table class="associative">
	<tr>
		<th>Type</th>
		<td><dfPlainText ref="component" /></td>
	</tr>

	<tr>
		<th>Date</th>
		<td><dfInput type="date" ref="date" required /></td>
	</tr>

	<tr>
		<th>Description</th>
		<td><dfInputTextArea ref="description" required /></td>
	</tr>

	<tr>
		<th>Tags</th>
		<td><dfInputList ref="tags"><ul>
			<dfListItem><li itemAttrs>
				<label> <dfInputCheckbox ref="item" /> <dfPlainText ref="name" /></label>
			</li></dfListItem>
		</ul></dfInputList></td>
	</tr>

	<tr>
		<th>Visibility</th>
		<td><ul>
			<!--<li><label><dfInputCheckbox ref="public" /> Public</label></li>
			<li><label><dfInputCheckbox ref="featured" /> Featured</label></li>-->
			<li><label><dfInputCheckbox ref="archived" /> Archived</label></li>
			</ul></td>
	</tr>
</table>