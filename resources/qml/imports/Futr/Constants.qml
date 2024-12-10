pragma Singleton
import QtQuick 2.15
import QtQuick.Controls.Material 2.15

QtObject {
    readonly property int width: 1280
    readonly property int height: 720

    // Spacing
    readonly property int spacing_xs: 4
    readonly property int spacing_s: 8
    readonly property int spacing_m: 16
    readonly property int spacing_l: 24
    readonly property int spacing_xl: 32

    // Base fonts
    readonly property font font: Qt.font({
        family: "Arial",
        pixelSize: Qt.application.font.pixelSize,
        weight: Font.Normal
    })

    readonly property font fontMedium: Qt.font({
        family: "Arial",
        pixelSize: Qt.application.font.pixelSize,
        weight: Font.Medium
    })

    readonly property font smallFont: Qt.font({
        family: "Arial",
        pixelSize: Qt.application.font.pixelSize * 0.8,
        weight: Font.Normal
    })

    readonly property font smallFontMedium: Qt.font({
        family: "Arial",
        pixelSize: Qt.application.font.pixelSize * 0.8,
        weight: Font.Medium
    })

    readonly property font largeFont: Qt.font({
        family: "Arial",
        pixelSize: Qt.application.font.pixelSize * 2,
        weight: Font.Normal
    })

    readonly property font largeFontMedium: Qt.font({
        family: "Arial",
        pixelSize: Qt.application.font.pixelSize * 2,
        weight: Font.Medium
    })

    readonly property color backgroundColor: Material.backgroundColor
}
