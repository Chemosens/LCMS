#'checkingIntegrationTable
#'This function checks whether duplicates exist in the integrationTable and allows to highlight potential mistakes in the integrationTable
#' @param integrationTable a table that must contain several columns: "compo" for the composition, "name" for the lipid species; "mz" for the theoretical exact mass/z (warnings: this exact mass has to vary depending on the mode NEG/POS of the acquisition); 
#' @examples 
#' data(integrationTable)
#' checkingIntegrationTable(integrationTable)
#'@export
checkingIntegrationTable=function(integrationTable)
{
  integrationTablePos2=integrationTable
  print("Duplicated compo ---------")
  nom_mol="compo"
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
  
  
  nom_mol="name"
  dup=integrationTablePos2[duplicated(integrationTablePos2[,nom_mol]),]
  print("Duplicated name  ---------")
  if(dim(dup)[1]!=0)
  {
    for(i in 1:dim(dup)[1])
    {
      compo_i=dup[i,nom_mol]
      print(integrationTablePos2[integrationTablePos2[,nom_mol]==compo_i,])
      print("----------")
    }
  }
}
