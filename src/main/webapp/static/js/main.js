$(function() {
    $("#local-id").on('input', function() {
        $("[data-name=local-id-span]").text($("#local-id").val())
    })
})