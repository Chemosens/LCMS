#'checkingIntegrationTable
#'This function checks whether duplicates exist in the integrationTable and allows to highlight potential mistakes in the integrationTable
#' @param integrationTable a table that must contain several columns: "compo" for the composition, "name" for the lipid species; "mz" for the theoretical exact mass/z (warnings: this exact mass has to vary depending on the mode NEG/POS of the acquisition); 
#' @param duplicatedName can be 'name' (default) or 'compo'. Detects if duplicated in this modality exists.
#' @examples 
#' data(integrationTable)
#' checkingIntegrationTable(integrationTable)
#'@export
checkingIntegrationTable=function(integrationTable,duplicatedName="name")
{
  integrationTablePos2=integrationTable
  print("Duplicated ---------")
  nom_mol=duplicatedName
  integrationTablePos3=integrationTablePos2[!is.na(integrationTablePos2[,nom_mol]),]
  dup=integrationTablePos3[duplicated(integrationTablePos3[,nom_mol]),]
  if(dim(dup)[1]!=0)
  {
    for(i in 1:dim(dup)[1])
    {
      print(i)
      compo_i=dup[i,nom_mol]
      print(integrationTablePos3[integrationTablePos3[,nom_mol]==compo_i,])
      print("----------")
    }
  }
  
}
