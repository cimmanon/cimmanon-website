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

			<form method="POST" action="./upload" enctype="multipart/form-data" class="simplified">
				<input type="file" name="form.file" multiple accept="image/*" /><br />
				<input type="submit" value="Upload" />
			</form>

			<table class="spreadsheet">
				<caption>Current Images</caption>

				<thead>
					<tr>
						<th>Filename</th>
						<th>Width</th>
						<th>Height</th>
					</tr>
				</thead>

				<tbody>
					<image><tr>
						<td><filename>example.jpg</filename></td>
						<td class="numeric"><width>480</width></td>
						<td class="numeric"><height>320</height></td>
					</tr></image>
			</table>
		</section>
	</div>
</apply>