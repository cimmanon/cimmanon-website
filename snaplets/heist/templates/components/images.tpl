<apply template="/_admin">
<h1>Images</h1>

<table class="spreadsheet">
	<caption>Current Images</caption>

	<thead>
		<tr>
			<th>Preview</th>
			<th>Filename</th>
			<th>Width</th>
			<th>Height</th>
		</tr>
	</thead>

	<tbody>
		<image><tr>
			<td><img src="/images/screenshots/${slug}/${filename}" width="200" alt="" /></td>
			<td><filename>example.jpg</filename></td>
			<td class="numeric"><width>480</width>px</td>
			<td class="numeric"><height>320</height>px</td>
		</tr></image>
	</tbody>
</table>

<dfForm class="simplified">
	<dfInputFile ref="file" multiple accept="image/*" /> <input type="submit" value="Upload" />
</dfForm>
</apply>