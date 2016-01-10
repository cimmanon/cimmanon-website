<apply template="/_admin">
<h1><name>Project Name</name> Components</h1>

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
			<td><component>Design</component></td>
			<td><date>2016-01-07</date></td>
			<td class="bool"><public>Public</public></td>
			<td><archived><a href="/archives/${slug}/${date}/">Archived</a></archived></td>
			<td><a href="./${component}/${date}/">Edit</a></td>
			<td><a href="./${component}/${date}/images">Images</a></td>
		</tr></component>
	</tbody>
</table>
</apply>