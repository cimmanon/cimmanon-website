<apply template="/_layout">
<h1><name>Project Name</name> Components</h1>

<div class="listing">
	<section>
		<h1>Current Components</h1>

		<table class="spreadsheet">
			<thead>
				<tr>
					<th>Component</th>
					<th>Date</th>
					<th>Public</th>
					<th>Archived</th>
					<th>Edit</th>
				</tr>
			</thead>

			<tbody>
				<component><tr class="${public}">
					<td><component>Design</component></td>
					<td><date>2016-01-07</date></td>
					<td class="bool"><public>Public</public></td>
					<td><archived><a href="/archives/${slug}/${date}/">Archived</a></archived></td>
					<td><a href="./${component}/${date}/">Edit</a></td>
				</tr></component>
			</tbody>
		</table>
	</section>

	<section>
		<h1>New Component</h1>

		<ul>
			<type><li><a href="./${name}"><name>Design</name></a></li></type>
		</ul>

		<dfForm class="simplified">
			<dfChildErrorList class="dialog error" />

			<apply template="_form" />
			<input type="submit" value="Add" />
		</dfForm>
	</section>
</div>
</apply>