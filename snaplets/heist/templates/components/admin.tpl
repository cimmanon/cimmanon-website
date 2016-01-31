<bind tag="pageTitle"><name>Project Name</name> Components</bind>
<apply template="/_admin">
<h1>Components</h1>

<table class="spreadsheet">
	<thead>
		<tr>
			<th>Component</th>
			<th>Date</th>
			<th>Public</th>
			<th>Archived</th>
			<th>Edit</th>
			<th>Images</th>
		</tr>
	</thead>

	<tbody>
		<component><tr class="${public}">
			<td><type>Design</type></td>
			<td><date>2016-01-07</date></td>
			<td class="bool"><public>Public</public></td>
			<td><archived><a href="/archives/${slug}/${date}/">Archived</a></archived></td>
			<td><a href="./${type}/${date}/">Edit</a></td>
			<td><a href="./${type}/${date}/images">Images</a></td>
		</tr></component>
	</tbody>
</table>
</apply>