defmodule SilverBrain.Service.SqlItemStore do
  use TypedStruct

  typedstruct do
    field :repo_name, String.t(), enforce: true
  end
end

defimpl SilverBrain.Core.ItemStore, for: SilverBrain.Service.SqlItemStore do
  alias SilverBrain.Service.Repo
  alias SilverBrain.Service.RepoManager
  alias SilverBrain.Service.Schema

  import Ecto.Query, only: [from: 2]

  def get_item(store, item_id, select) do
    IO.inspect(select)

    with :ok <- RepoManager.connect(store.repo_name) do
      Repo.all(from(Schema.Item, where: [id: ^item_id], select: ^select))
    end
  end

  def get_items(store, item_ids, select) do
  end

  def create_item(store, name) do
  end

  def update_item(store, item) do
  end

  def delete_item(store, item_id) do
  end

  # ============================================================
  #  Attachments
  # ============================================================

  alias SilverBrain.Core.Attachment

  def get_attachments(store, item_id) do
  end

  def remove_attachment(store, item_id, attachment_id) do
  end

  # ============================================================
  #  Link
  # ============================================================

  def create_child(store, item_id, child_id) do
  end

  def get_parents(store, item_id) do
  end

  def get_children(store, item_id) do
  end

  def delete_child(store, item_id, child_id) do
  end
end
