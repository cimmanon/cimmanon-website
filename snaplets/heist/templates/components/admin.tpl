<bind tag="pageTitle"><name>Project Name</name> Components</bind>
<apply template="/_admin">
<h1>Components</h1>

<table class="spreadsheet">
	<thead>
		<tr>
			<th>Component</th>
			<th>Date</th>
			<th>Public</th>
			<th>Featured</th>
			<th>Archived</th>
			<th>Edit</th>
			<th>Image</th>
		</tr>
	</thead>

	<tbody>
		<component><tr class="${public}">
			<td><type>Design</type></td>
			<td><date>2016-01-07</date></td>
			<td class="bool"><public>Public</public></td>
			<td class="bool"><featured>Featured</featured></td>
			<td><archived><a href="${href}">Archived</a></archived></td>
			<td><a href="./${type}/${date}/">Edit</a></td>
			<td><a href="./${type}/${date}/images" title="Image Administration"><image>
				<yes><img src="/screenshots/${slug}/${filename}" width="200" alt="Featured Image" /></yes>
				<no>Add Images</no>
			</image></a></td>
		</tr></component>
	</tbody>
</table>
</apply>
