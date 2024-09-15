defprotocol SilverBrain.Core.AttachmentStore do
  alias SilverBrain.Core.Attachment
  @type result(t) :: SilverBrain.Core.result(t)
  @type result :: SilverBrain.Core.result()

  @spec get_attachment(t, Attachment.id()) :: result(Attachment.t())
  def get_attachment(store, attachment_id)

  @spec create_attachment(t, String.t(), Path.t()) :: result(Attachment.id())
  def create_attachment(store, attachment_name, file_path)

  @spec update_attachment(t, Attachment.t()) :: result()
  def update_attachment(store, attachment)

  @spec delete_attachment(t, Attachment.id()) :: result()
  def delete_attachment(store, attachment_id)
end
