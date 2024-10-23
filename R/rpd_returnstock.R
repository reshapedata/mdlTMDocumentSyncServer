#' 生产退库同步至数据中台
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#' @param wms_token
#'
#' @return 返回值
#' @export
#'
#' @examples
#' rpd_returnstock_sync_dmsServer()
rpd_returnstock_sync_dmsServer <- function(input,output,session,dms_token,wms_token) {

  txt_rpd_returnstock_fyear_wms = tsui::var_text('txt_rpd_returnstock_fyear_wms')

  txt_rpd_returnstock_fmonth_wms = tsui::var_text('txt_rpd_returnstock_fmonth_wms')
  shiny::observeEvent(input$btn_rpd_returnstock_sync_dms,{
    fyear =txt_rpd_returnstock_fyear_wms()
    fmonth=txt_rpd_returnstock_fmonth_wms()


    if(fyear==''||fmonth==''){
      tsui::pop_notice("请输入年份和月份")
    }
    else {
      data= mdlTMDocumentSyncPkg::TmWMS_rpd_returnstock_sekectBymonth(wms_token =wms_token ,fyear = fyear,fmonth = fmonth)

      data = as.data.frame(data)

      data = tsdo::na_standard(data)

      mdlTMDocumentSyncPkg::TmWMS_rpd_returnstock_deleteBymonth(dms_token = dms_token,fyear =fyear ,fmonth =fmonth )

      tsda::db_writeTable2(token = dms_token,table_name = 'rds_dms_src_t_rpd_returnstock',r_object = data,append = TRUE)

      tsui::pop_notice("上传完成")

    }





  })


}


#' 生产退库同步至ERP
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' rpd_returnstock_sync_erpServer()
rpd_returnstock_sync_erpServer <- function(input,output,session,dms_token) {

  txt_rpd_returnstock_fbillno_sync_erp = tsui::var_text('txt_rpd_returnstock_fbillno_sync_erp')
  shiny::observeEvent(input$btn_rpd_returnstock_sync_erp,{
    tsui::pop_notice(txt_rpd_returnstock_fbillno_sync_erp())
    print("功能待完善")




  })


}




#' DMS生产退库按单据编号查询
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' rpd_returnstock_fbillno_view_dmsServer()
rpd_returnstock_fbillno_view_dmsServer <- function(input,output,session,dms_token) {


  txt_rpd_returnstock_fbillno_view = tsui::var_text('txt_rpd_returnstock_fbillno_view')
  shiny::observeEvent(input$btn_rpd_returnstock_fbillno_view_dms,{

    fbillno=txt_rpd_returnstock_fbillno_view()

    data=mdlTMDocumentSyncPkg::Tmdms_rpd_returnstock_selectByfbillno(dms_token = dms_token,fbillno =fbillno)

    tsui::run_dataTable2(id = 'dt_rpd_returnstock',data = data)
    tsui::run_download_xlsx(id = 'dl_rpd_returnstock_fbillno_dms',data = data,filename = "生产退库按单查询数据.xlsx")




  })


}

#' 生产退库按日期查询
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' rpd_returnstock_date_view_dmsServer()
rpd_returnstock_date_view_dmsServer <- function(input,output,session,dms_token) {

  date_tm_rpd_returnstock= tsui::var_date('date_tm_rpd_returnstock')

  shiny::observeEvent(input$btn_rpd_returnstock_date_view_dms,{
    fdate=date_tm_rpd_returnstock()
    data=mdlTMDocumentSyncPkg::Tmdms_rpd_returnstock_selectBydate(dms_token = dms_token,fdate =fdate )

    tsui::run_dataTable2(id = 'dt_rpd_returnstock',data = data)

  })


}

#' 生产退库按日期范围查询
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' rpd_returnstock_dateRange_view_dmsServer()
rpd_returnstock_dateRange_view_dmsServer <- function(input,output,session,dms_token) {

  date_tm_rpd_returnstock_FStartDate= tsui::var_date('date_tm_rpd_returnstock_FStartDate')

  date_tm_rpd_returnstock_FEndDate= tsui::var_date('date_tm_rpd_returnstock_FEndDate')

  shiny::observeEvent(input$btn_rpd_returnstock_dateRange_view_dms,{
    FStartDate=date_tm_rpd_returnstock_FStartDate()

    FEndDate=date_tm_rpd_returnstock_FEndDate()

    data=mdlTMDocumentSyncPkg::Tmdms_rpd_returnstock_selectBydateRange(dms_token =dms_token ,FStartDate =FStartDate ,FEndDate = FEndDate)


    tsui::run_dataTable2(id = 'dt_rpd_returnstock',data = data)

    tsui::run_download_xlsx(id = 'dl_rpd_returnstock_dateRange_dms',data = data,filename = "生产退库单数据.xlsx")





  })


}


#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#' @param wms_token
#' @return 返回值
#' @export
#'
#' @examples
#' rpd_returnstockServer()
rpd_returnstockServer <- function(input,output,session,dms_token,wms_token) {


  mdlTMDocumentSyncServer::rpd_returnstock_sync_dmsServer(input =input ,output =output ,session = session,dms_token =dms_token ,wms_token = wms_token)

  mdlTMDocumentSyncServer::rpd_returnstock_sync_erpServer(input =input ,output =output ,session = session,dms_token =dms_token )


  mdlTMDocumentSyncServer::rpd_returnstock_fbillno_view_dmsServer(input =input ,output =output ,session = session,dms_token =dms_token )

  mdlTMDocumentSyncServer::rpd_returnstock_date_view_dmsServer(input =input ,output =output ,session = session,dms_token =dms_token )

  mdlTMDocumentSyncServer::rpd_returnstock_dateRange_view_dmsServer(input =input ,output =output ,session = session,dms_token =dms_token )


}
