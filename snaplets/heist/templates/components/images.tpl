<bind tag="pageTitle"><name>Project Name</name> Images</bind>
<apply template="/_admin">
<h1>Images</h1>

<dfForm method="post">
<dfChildErrorList class="dialog error" />

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
		<image><tr>
			<td><img src="/screenshots/${slug}/${filename}" width="200" alt="" /></td>
			<td><filename>example.jpg</filename></td>
			<td class="numeric"><width>480</width>px</td>
			<td class="numeric"><height>320</height>px</td>
			<td class="fixed"><label><input type="radio" name="update.featured" value="${indice}" isFeatured /> Featured</label></td>
			<td class="fixed"><label><input type="checkbox" name="update.delete" value="${indice}" /> Delete</label></td>
		</tr></image>
	</tbody>
</table>
<input type="submit" value="Update" />
</dfForm>

<h2>Upload</h2>

<form method="post" action="./upload" enctype="multipart/form-data" class="simplified">
	<input type="file" name="upload.file" multiple accept="image/*" /> <input type="submit" value="Upload" />
</form>
</apply>
