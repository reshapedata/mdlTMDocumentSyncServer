#' 生产领料_倒扣线边库同步至数据中台
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
#' prd_pickmtr_sync_dmsServer()
prd_pickmtr_sync_dmsServer <- function(input,output,session,dms_token,wms_token) {

  txt_prd_pickmtr_fyear_wms = tsui::var_text('txt_prd_pickmtr_fyear_wms')

  txt_prd_pickmtr_fmonth_wms = tsui::var_text('txt_prd_pickmtr_fmonth_wms')
  shiny::observeEvent(input$btn_prd_pickmtr_sync_dms,{
    fyear =txt_prd_pickmtr_fyear_wms()
    fmonth=txt_prd_pickmtr_fmonth_wms()
    print("1")

    if(fyear==''||fmonth==''){
      tsui::pop_notice("请输入年份和月份")
    }
    else {
      data= mdlTMDocumentSyncPkg::TmWMS_prd_pickmtr_sekectBymonth(wms_token =wms_token ,fyear = fyear,fmonth = fmonth)
      print("2")
      data = as.data.frame(data)

      data = tsdo::na_standard(data)

      mdlTMDocumentSyncPkg::TmWMS_prd_pickmtr_deleteBymonth(dms_token = dms_token,fyear =fyear ,fmonth =fmonth )
      print("3")

      tsda::db_writeTable2(token = dms_token,table_name = 'rds_dms_src_t_prd_pickmtrl_inverted',r_object = data,append = TRUE)
      print("4")
      tsui::pop_notice("上传完成")

    }





  })


}


#' 生产领料_倒扣线边库同步至ERP
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
#' prd_pickmtr_sync_erpServer()
prd_pickmtr_sync_erpServer <- function(input,output,session,dms_token) {

  txt_prd_pickmtr_fbillno_sync_erp = tsui::var_text('txt_prd_pickmtr_fbillno_sync_erp')
  shiny::observeEvent(input$btn_prd_pickmtr_sync_erp,{
    tsui::pop_notice(txt_prd_pickmtr_fbillno_sync_erp())
    print("功能待完善")




  })


}




#' DMS生产领料_倒扣线边库按单据编号查询
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
#' prd_pickmtr_fbillno_view_dmsServer()
prd_pickmtr_fbillno_view_dmsServer <- function(input,output,session,dms_token) {


  txt_prd_pickmtr_fbillno_view = tsui::var_text('txt_prd_pickmtr_fbillno_view')
  shiny::observeEvent(input$btn_prd_pickmtr_fbillno_view_dms,{

    fbillno=txt_prd_pickmtr_fbillno_view()

    data=mdlTMDocumentSyncPkg::Tmdms_prd_pickmtr_selectByfbillno(dms_token = dms_token,fbillno =fbillno)

    tsui::run_dataTable2(id = 'dt_prd_pickmtr',data = data)
    tsui::run_download_xlsx(id = 'dl_prd_pickmtr_fbillno_dms',data = data,filename = "生产领料_倒扣线边库按单查询数据.xlsx")




  })


}

#' 生产领料_倒扣线边库按日期查询
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
#' prd_pickmtr_date_view_dmsServer()
prd_pickmtr_date_view_dmsServer <- function(input,output,session,dms_token) {

  date_tm_prd_pickmtr= tsui::var_date('date_tm_prd_pickmtr')

  shiny::observeEvent(input$btn_prd_pickmtr_date_view_dms,{
    fdate=date_tm_prd_pickmtr()
    data=mdlTMDocumentSyncPkg::Tmdms_prd_pickmtr_selectBydate(dms_token = dms_token,fdate =fdate )

    tsui::run_dataTable2(id = 'dt_prd_pickmtr',data = data)

  })


}

#' 生产领料_倒扣线边库按日期范围查询
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
#' prd_pickmtr_dateRange_view_dmsServer()
prd_pickmtr_dateRange_view_dmsServer <- function(input,output,session,dms_token) {

  date_tm_prd_pickmtr_FStartDate= tsui::var_date('date_tm_prd_pickmtr_FStartDate')

  date_tm_prd_pickmtr_FEndDate= tsui::var_date('date_tm_prd_pickmtr_FEndDate')

  shiny::observeEvent(input$btn_prd_pickmtr_dateRange_view_dms,{
    FStartDate=date_tm_prd_pickmtr_FStartDate()

    FEndDate=date_tm_prd_pickmtr_FEndDate()

    data=mdlTMDocumentSyncPkg::Tmdms_prd_pickmtr_selectBydateRange(dms_token =dms_token ,FStartDate =FStartDate ,FEndDate = FEndDate)


    tsui::run_dataTable2(id = 'dt_prd_pickmtr',data = data)

    tsui::run_download_xlsx(id = 'dl_prd_pickmtr_dateRange_dms',data = data,filename = "生产领料_倒扣线边库数据.xlsx")





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
#' prd_pickmtrServer()
prd_pickmtrServer <- function(input,output,session,dms_token,wms_token) {


  mdlTMDocumentSyncServer::prd_pickmtr_sync_dmsServer(input =input ,output =output ,session = session,dms_token =dms_token ,wms_token = wms_token)

  mdlTMDocumentSyncServer::prd_pickmtr_sync_erpServer(input =input ,output =output ,session = session,dms_token =dms_token )


  mdlTMDocumentSyncServer::prd_pickmtr_fbillno_view_dmsServer(input =input ,output =output ,session = session,dms_token =dms_token )

  mdlTMDocumentSyncServer::prd_pickmtr_date_view_dmsServer(input =input ,output =output ,session = session,dms_token =dms_token )

  mdlTMDocumentSyncServer::prd_pickmtr_dateRange_view_dmsServer(input =input ,output =output ,session = session,dms_token =dms_token )


}
