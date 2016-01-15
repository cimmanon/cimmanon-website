<bind tag="pageTitle"><name>Project Name</name> Images</bind>
<apply template="/_admin">
<h1>Images</h1>

<form method="post">
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
			<td class="fixed"><label><isDefault>
				<yes><input type="radio" name="update.featured" value="${filename}" checked /></yes>
				<no><input type="radio" name="update.featured" value="${filename}" /></no>
			</isDefault> Default</label></td>
			<td class="fixed"><label><input type="checkbox" name="update.delete.${indice}.item" /> Delete</label></td>
		</tr></image>
	</tbody>
</table>
<input type="submit" value="Update" />
</form>

<h2>Upload</h2>

<form method="post" action="./upload" enctype="multipart/form-data" class="simplified">
	<input type="file" name="upload.file" multiple accept="image/*" /> <input type="submit" value="Upload" />
</form>
</apply>