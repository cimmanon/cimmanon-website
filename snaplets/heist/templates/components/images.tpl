<bind tag="pageSubTitle">Images</bind>
<apply template="_admin">
<h1>Images</h1>

<flash type="success" />

<dfForm method="post">
<dfChildErrorList class="dialog error" />

<dfInputListCustom ref="images">
<table class="spreadsheet">
	<caption>Current Images</caption>

	<thead>
		<tr>
			<th>Preview</th>
			<th>Filename</th>
			<th>Width</th>
			<th>Height</th>
			<th>Featured</th>
			<th>Delete</th>
		</tr>
	</thead>

	<tbody>
		<dfListItem only="items"><tr wrapperAttrs>
			<td><dfCustomText ref="filename"><img src="/screenshots/${slug}/${value}" width="200" alt="" /></dfCustomText></td>
			<td><dfPlainText ref="filename">example.jpg</dfPlainText></td>
			<td class="numeric"><dfPlainText ref="width">480</dfPlainText>px</td>
			<td class="numeric"><dfPlainText ref="height">320</dfPlainText>px</td>
			<td class="fixed"><label><dfGroupRadioText ref="filename" checked="featured" /> Featured</label></td>
			<td class="fixed"><label><dfInputCheckbox ref="delete" /> Delete</label></td>
		</tr></dfListItem>
	</tbody>
</table>
</dfInputListCustom>
<input type="submit" value="Update" />
</dfForm>

<h2>Upload</h2>

<form method="post" action="./upload" enctype="multipart/form-data" class="simplified">
	<input type="file" name="upload.file" multiple accept="image/*" /> <input type="submit" value="Upload" />
</form>
</apply>
