<apply template="/_layout">
	<h1>Edit Component</h1>

	<div class="listing">
		<section>
			<h1>Details</h1>

			<dfForm class="simplified">
				<apply template="_form" />
				<input type="submit" value="Edit" />
			</dfForm>
		</section>

		<section>
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
			</table>

			<form method="POST" action="./upload" enctype="multipart/form-data" class="simplified">
				<input type="file" name="form.file" multiple accept="image/*" /> <input type="submit" value="Upload" />
			</form>
		</section>
	</div>
</apply>