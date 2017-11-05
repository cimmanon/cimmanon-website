<bind tag="pageTitle">Tags</bind>
<apply template="/_admin">
<h1>Tags</h1>

<flash type="success" />

<dfForm>
	<dfChildErrorList class="dialog error" />

	<dfInputListStatic ref="">
		<dfListItem>
			<dfInputListCustom ref="tags"><fieldset listAttrs>
				<legend><dfPlainText ref="component_type">Design</dfPlainText></legend>

				<indices />
				<ul>
					<dfListItem><li wrapperAttrs>
						<dfInputSelect ref="category" />
						<dfInputText ref="tag" />
						<input type="button" name="${dfPath}.remove" value="Remove" />
					</li></dfListItem>
				</ul>
				<input type="button" name="add" value="Add" />
			</fieldset></dfInputListCustom>
		</dfListItem>
	</dfInputListStatic>

	<input type="submit" value="Save" />
</dfForm>
<script type="text/javascript" src="/javascript/df-list.js"></script>
</apply>
